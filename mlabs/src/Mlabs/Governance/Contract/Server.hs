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
import Mlabs.Governance.Contract.Validation (AssetClassNft(..), AssetClassGov(..), GovernanceDatum(..), GovernanceRedeemer(..))
import Mlabs.Plutus.Contract (getEndpoint, selects)

-- do we want another error type? 
type GovernanceContract a = Contract.Contract (Maybe (Last Integer)) Api.GovernanceSchema Text a

governanceEndpoints :: AssetClassNft -> AssetClassGov -> GovernanceContract ()
governanceEndpoints nft gov = do
  -- some nft gov duplication here, probably have to refactor all
  -- of the Api types to hold nft and gov themselves. TBD (do we want them as params or not?)
  -- getEndpoint @Api.StartGovernance >>= startGovernance --FIXME temporary moved to selects to make tests work
  forever $ selects
    [ getEndpoint @Api.StartGovernance >>= startGovernance 
    , getEndpoint @Api.Deposit >>= deposit nft gov
    , getEndpoint @Api.Withdraw >>= withdraw nft gov
    , getEndpoint @Api.ProvideRewards >>= provideRewards nft gov
    , getEndpoint @Api.QueryBalance >>= queryBalance nft gov
    ]

--- actions

startGovernance :: Api.StartGovernance -> GovernanceContract ()
startGovernance (Api.StartGovernance nft gov) = do
  let d = GovernanceDatum (Validation.GRWithdraw "" 0) AssocMap.empty
      v = singleton (acNftCurrencySymbol nft) (acNftTokenName nft) 1
      tx = Constraints.mustPayToTheScript d v
  ledgerTx <- Contract.submitTxConstraints (Validation.scrInstance nft gov) tx
  void $ Contract.awaitTxConfirmed $ txId ledgerTx
  Contract.logInfo @String $ printf "Started governance for nft token %s, gov token %s" (show nft) (show gov)

deposit :: AssetClassNft -> AssetClassGov -> Api.Deposit -> GovernanceContract ()
deposit nft gov (Api.Deposit amnt) = do
  pkh <- pubKeyHash <$> Contract.ownPubKey
  (datum, utxo, oref) <- findGovernance nft gov

  let traceNFT = singleton (acNftCurrencySymbol nft) (acNftTokenName nft) 1
      xGovValue = Validation.xgovSingleton nft (coerce pkh) amnt
      datum' = GovernanceDatum (Validation.GRDeposit pkh amnt) $
        case AssocMap.lookup pkh (gdDepositMap datum) of
          Nothing -> AssocMap.insert pkh amnt (gdDepositMap datum)
          Just n  -> AssocMap.insert pkh (n+amnt) (gdDepositMap datum)
      tx = sconcat [
          Constraints.mustMintValue               xGovValue
        , Constraints.mustPayToTheScript datum' $ Validation.govSingleton gov amnt <> traceNFT
        , Constraints.mustSpendScriptOutput oref  (Redeemer . toBuiltinData $ GRDeposit pkh amnt)
        ]
      lookups = sconcat [
              Constraints.mintingPolicy          $ Validation.xGovMintingPolicy nft
            , Constraints.otherScript            $ Validation.scrValidator nft gov
            , Constraints.typedValidatorLookups  $ Validation.scrInstance nft gov
            , Constraints.unspentOutputs         $ Map.singleton oref utxo
            ]
                
  ledgerTx <- Contract.submitTxConstraintsWith @Validation.Governance lookups tx
  void $ Contract.awaitTxConfirmed $ txId ledgerTx
  Contract.logInfo @String $ printf "deposited %s GOV tokens" (show amnt)

withdraw :: AssetClassNft -> AssetClassGov -> Api.Withdraw -> GovernanceContract ()
withdraw nft gov (Api.Withdraw val) = do
  -- 'guard' doesn't work here
  if [acGovCurrencySymbol gov] == (AssocMap.keys $ getValue val) then
    Contract.throwError "Attempt to withdraw with non xGOV tokens"
  else
    pure ()

  pkh <- pubKeyHash <$> Contract.ownPubKey
  (datum, _, oref) <- findGovernance nft gov
  tokens <- fmap AssocMap.toList . maybe (Contract.throwError "No xGOV tokens found") pure
            . AssocMap.lookup (Validation.xGovCurrencySymbol nft) $ getValue val
  let maybemap' :: Maybe (AssocMap.Map PubKeyHash Integer)
      maybemap' = foldM (\mp (tn, amm) -> withdrawFromCorrect tn amm mp) (gdDepositMap datum) tokens

      totalPaid :: Integer
      totalPaid = sum . map snd $ tokens

      -- AssocMap has no "insertWith", so we have to use lookup and insert, all under foldM
      withdrawFromCorrect tn amm mp =
        case AssocMap.lookup pkh mp of
          Just n | n > amm  -> Just (AssocMap.insert depositor (n-amm) mp)
          Just n | n == amm -> Just (AssocMap.delete depositor mp)
          _                 -> Nothing
          where depositor = coerce tn
          
  datum' <- GovernanceDatum (Validation.GRWithdraw pkh totalPaid)
    <$> maybe (Contract.throwError "Minting policy unsound OR invalid input") pure maybemap'
  
  let totalGov = sum $ map snd tokens
      tx = sconcat [
        -- user doesn't pay to script, but instead burns the xGOV (ensured by validators)
          Constraints.mustPayToTheScript datum' mempty
        , Constraints.mustMintValue (negate val)
        , Constraints.mustPayToPubKey pkh $ Validation.govSingleton gov totalGov
        , Constraints.mustSpendScriptOutput oref (Redeemer . toBuiltinData $ GRWithdraw pkh totalGov)
        ]
      lookups = sconcat [
              Constraints.typedValidatorLookups $ Validation.scrInstance nft gov
            , Constraints.otherScript           $ Validation.scrValidator nft gov
            , Constraints.mintingPolicy         $ Validation.xGovMintingPolicy nft
            ]
                
  ledgerTx <- Contract.submitTxConstraintsWith @Validation.Governance lookups tx
  void $ Contract.awaitTxConfirmed $ txId ledgerTx
  Contract.logInfo @String $ printf "withdrew %s GOV tokens" (show totalGov)

provideRewards :: AssetClassNft -> AssetClassGov -> Api.ProvideRewards -> GovernanceContract ()
provideRewards nft gov (Api.ProvideRewards val) = do
  (datum, _, _) <- findGovernance nft gov
  let -- annotates each depositor with the total percentage of GOV deposited to the contract 
      (total, props) = foldr (\(pkh, amm) (t, p) -> (amm+t, (pkh, amm%total):p)) (0, []) $ AssocMap.toList (gdDepositMap datum)
      dispatch = map (\(pkh, prop) -> (pkh,Value $ fmap (round.(prop *).(%1)) <$> getValue val)) props

  let tx = foldMap (uncurry Constraints.mustPayToPubKey) dispatch
      lookups = sconcat [
              Constraints.otherScript $ Validation.scrValidator nft gov
            ]

  ledgerTx <- Contract.submitTxConstraintsWith @Validation.Governance lookups tx
  void $ Contract.awaitTxConfirmed $ txId ledgerTx
  Contract.logInfo @String $ printf "Provided rewards to all xGOV holders"  

queryBalance :: AssetClassNft -> AssetClassGov -> Api.QueryBalance -> GovernanceContract ()
queryBalance nft gov (Api.QueryBalance pkh) = do
  (datum,_,_) <- findGovernance nft gov
  Contract.tell . fmap Last $ AssocMap.lookup pkh (gdDepositMap datum)
  
--- util

-- assumes the Governance is parametrised by an NFT.
findGovernance :: AssetClassNft -> AssetClassGov -> GovernanceContract (Validation.GovernanceDatum, TxOutTx, TxOutRef)
findGovernance nft gov = do
  utxos <- Contract.utxoAt $ Validation.scrAddress nft gov
  let xs = [ (oref, o)
           | (oref, o) <- Map.toList utxos
           , valueOf (txOutValue $ txOutTxOut o) (acNftCurrencySymbol nft) (acNftTokenName nft) == 1
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
