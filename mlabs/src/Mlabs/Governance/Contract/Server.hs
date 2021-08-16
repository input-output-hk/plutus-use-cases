{-# LANGUAGE OverloadedLists #-}

-- | Server for governance application
module Mlabs.Governance.Contract.Server (
    GovernanceContract
  , governanceEndpoints
  ) where

import PlutusTx.Prelude hiding (toList)
import Prelude (String, uncurry, show)

import Data.Text (Text)
import Data.Map qualified as Map
import Data.Coerce (coerce)
import PlutusTx.AssocMap qualified as AssocMap
import Text.Printf (printf)
import Control.Monad (forever, void, foldM)
import Data.Semigroup (Last(..), sconcat)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Crypto (pubKeyHash, PubKeyHash(..))
import Plutus.V1.Ledger.Api (fromBuiltinData, toBuiltinData, Datum(..), Redeemer(..))
import Plutus.V1.Ledger.Tx (txId, TxOutRef, TxOutTx(..), Tx(..), TxOut(..))
import Plutus.V1.Ledger.Value (Value(..), TokenName(..), valueOf, singleton)
import Ledger.Constraints qualified as Constraints
import Mlabs.Governance.Contract.Api qualified as Api
import Mlabs.Governance.Contract.Validation qualified as Validation
import Mlabs.Governance.Contract.Validation (GovParams(..), GovernanceDatum(..), GovernanceRedeemer(..))
import Mlabs.Plutus.Contract (getEndpoint, selects)
import Mlabs.Data.List (sortOn)

type GovernanceContract a = Contract.Contract (Maybe (Last Integer)) Api.GovernanceSchema Text a

governanceEndpoints :: GovParams -> GovernanceContract ()
governanceEndpoints params = forever $ selects
    [ getEndpoint @Api.StartGovernance >>= startGovernance 
    , getEndpoint @Api.Deposit >>= deposit params
    , getEndpoint @Api.Withdraw >>= withdraw params
    , getEndpoint @Api.ProvideRewards >>= provideRewards params
    , getEndpoint @Api.QueryBalance >>= queryBalance params
    ]

--- actions

startGovernance :: Api.StartGovernance -> GovernanceContract ()
startGovernance (Api.StartGovernance params) = do
  let d = GovernanceDatum (Validation.GRWithdraw "" 0) AssocMap.empty
      v = singleton params.nft.acNftCurrencySymbol params.nft.acNftTokenName 1
      tx = Constraints.mustPayToTheScript d v
  ledgerTx <- Contract.submitTxConstraints (Validation.scrInstance params) tx
  void $ Contract.awaitTxConfirmed $ txId ledgerTx
  Contract.logInfo @String $ printf "Started governance for nft token %s, gov token %s" (show params.nft) (show params.gov)

deposit :: GovParams -> Api.Deposit -> GovernanceContract ()
deposit params (Api.Deposit amnt) = do
  ownPkh <- pubKeyHash <$> Contract.ownPubKey
  (datum, utxo, oref) <- findGovernance params
  let traceNFT = singleton params.nft.acNftCurrencySymbol params.nft.acNftTokenName 1
      xGovValue = Validation.xgovSingleton params.nft (coerce ownPkh) amnt
      datum' = GovernanceDatum 
                (Validation.GRDeposit ownPkh amnt)
                (updateAmount ownPkh amnt datum.gdDepositMap)
      tx = sconcat [
          Constraints.mustMintValue               xGovValue
        , Constraints.mustPayToTheScript datum' $ Validation.govSingleton params.gov amnt <> txOutValue (txOutTxOut utxo)
        , Constraints.mustSpendScriptOutput oref  (Redeemer . toBuiltinData $ GRDeposit ownPkh amnt)
        ]
      lookups = sconcat [
              Constraints.mintingPolicy          $ Validation.xGovMintingPolicy params.nft
            , Constraints.otherScript            $ Validation.scrValidator params
            , Constraints.typedValidatorLookups  $ Validation.scrInstance params
            , Constraints.unspentOutputs         $ Map.singleton oref utxo
            ]
                
  ledgerTx <- Contract.submitTxConstraintsWith @Validation.Governance lookups tx
  void $ Contract.awaitTxConfirmed $ txId ledgerTx
  Contract.logInfo @String $ printf "deposited %s GOV tokens" (show amnt)
  where
    updateAmount pkh amount depositMap = 
      let amount' = amount + fromMaybe 0 (AssocMap.lookup pkh depositMap) 
      in AssocMap.insert pkh amount' depositMap

withdraw ::GovParams -> Api.Withdraw -> GovernanceContract ()
withdraw params (Api.Withdraw val) = do
  ownPkh              <- pubKeyHash <$> Contract.ownPubKey
  (datum, utxo, oref) <- findGovernance params
  Contract.logInfo @String $ "@@ value at utxo: " ++ show (txOutValue $ txOutTxOut utxo)
  tokens              <- findTokens val
  let 
      scriptBalance  = txOutValue $ txOutTxOut utxo
      toWalletGovAmt = sum $ map snd tokens
      toWalletValue  = Validation.govSingleton params.gov toWalletGovAmt
  datum' <- updateDatumDepositMap ownPkh datum tokens toWalletGovAmt
  let 
      tx = sconcat [
          -- pay overall balance minus GOV withdraw amount back to script  
          Constraints.mustPayToTheScript datum' $ scriptBalance - toWalletValue
          -- ensures that we won't withdraw negitve amount
        , Constraints.mustPayToPubKey ownPkh toWalletValue
        , Constraints.mustMintValue (negate val) -- burn xGOV tokens
        , Constraints.mustSpendScriptOutput oref (Redeemer . toBuiltinData $ GRWithdraw ownPkh toWalletGovAmt)
        ]
      lookups = sconcat [
              Constraints.typedValidatorLookups $ Validation.scrInstance params
            , Constraints.otherScript           $ Validation.scrValidator params
            , Constraints.mintingPolicy         $ Validation.xGovMintingPolicy params.nft
            , Constraints.unspentOutputs        $ Map.singleton oref utxo
            ]
  ledgerTx <- Contract.submitTxConstraintsWith @Validation.Governance lookups tx
  void $ Contract.awaitTxConfirmed $ txId ledgerTx
  Contract.logInfo @String $ printf "withdrew %s GOV tokens" (show toWalletGovAmt)
  where
    findTokens v = 
      fmap AssocMap.toList . maybe (Contract.throwError "No xGOV tokens found") pure
      . AssocMap.lookup (Validation.xGovCurrencySymbol params.nft) $ getValue v

    sortMap = AssocMap.fromList . sortOn fst . AssocMap.toList

    -- TODO try to simplify
    updateDatumDepositMap pkh datum tokens toWalletGovAmt = 
      GovernanceDatum (Validation.GRWithdraw pkh toWalletGovAmt) . sortMap
        <$> maybe (Contract.throwError "Minting policy unsound OR invalid input") pure maybemap'
      where
        maybemap' :: Maybe (AssocMap.Map PubKeyHash Integer)
        maybemap' = foldM (\mp (tn, amm) -> withdrawFromCorrect tn amm mp) (gdDepositMap datum) tokens
        -- AssocMap has no "insertWith", so we have to use lookup and insert, all under foldM
        withdrawFromCorrect tn amm mp =
          case AssocMap.lookup depositor mp of
            Just n | n > amm  -> Just (AssocMap.insert depositor (n-amm) mp)
            Just n | n == amm -> Just (AssocMap.delete depositor mp)
            _                 -> Nothing
            where depositor = coerce tn

provideRewards :: GovParams -> Api.ProvideRewards -> GovernanceContract ()
provideRewards params (Api.ProvideRewards val) = do
  (datum, _, _) <- findGovernance params
  let -- annotates each depositor with the total percentage of GOV deposited to the contract 
      (total, props) = foldr (\(pkh, amm) (t, p) -> (amm+t, (pkh, amm%total):p)) (0, []) $ AssocMap.toList (gdDepositMap datum)
      dispatch = map (\(pkh, prop) -> (pkh,Value $ fmap (round.(prop *).(%1)) <$> getValue val)) props

  let tx = foldMap (uncurry Constraints.mustPayToPubKey) dispatch
      lookups = sconcat [
              Constraints.otherScript $ Validation.scrValidator params
            ]

  ledgerTx <- Contract.submitTxConstraintsWith @Validation.Governance lookups tx
  void $ Contract.awaitTxConfirmed $ txId ledgerTx
  Contract.logInfo @String $ printf "Provided rewards to all xGOV holders"  

queryBalance :: GovParams -> Api.QueryBalance -> GovernanceContract ()
queryBalance params (Api.QueryBalance pkh) = do
  (datum,_,_) <- findGovernance params
  Contract.tell . fmap Last $ AssocMap.lookup pkh (gdDepositMap datum)
  
--- util

-- assumes the Governance is parametrised by an NFT.
findGovernance :: GovParams -> GovernanceContract (Validation.GovernanceDatum, TxOutTx, TxOutRef)
findGovernance params = do
  utxos <- Contract.utxoAt $ Validation.scrAddress params
  let xs = [ (oref, o)
           | (oref, o) <- Map.toList utxos
           , valueOf (txOutValue $ txOutTxOut o) params.nft.acNftCurrencySymbol params.nft.acNftTokenName == 1
           ]
  case xs of
    [(oref, o)] -> case txOutDatumHash $ txOutTxOut o of
      Nothing -> Contract.throwError "unexpected out type"
      Just h  -> case Map.lookup h $ txData $ txOutTxTx o of
        Nothing        -> Contract.throwError "datum not found"
        Just (Datum e) -> case fromBuiltinData e of
          Nothing -> Contract.throwError "datum has wrong type"
          Just gd -> return (gd, o, oref)
    _ -> Contract.throwError "No UTxO found"
