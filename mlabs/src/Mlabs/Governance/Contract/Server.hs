{-# LANGUAGE OverloadedLists #-}

-- | Server for governance application
module Mlabs.Governance.Contract.Server (
    GovernanceContract
  , governanceEndpoints
  ) where

import PlutusTx.Prelude hiding (toList)

import Data.Text (Text)
import Data.Map qualified as Map
import Data.Coerce (coerce)
import PlutusTx.AssocMap qualified as AssocMap
import Text.Printf (printf)
import Control.Monad (forever, void, foldM)
import Data.Semigroup (Last(..), sconcat)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Crypto (pubKeyHash, PubKeyHash(..))
import Plutus.V1.Ledger.Contexts (scriptCurrencySymbol)
import Plutus.V1.Ledger.Api (fromData, toData, Datum(..), Redeemer(..))
import Plutus.V1.Ledger.Tx (txId, TxOutRef, TxOutTx(..), Tx(..), TxOut(..))
import Plutus.V1.Ledger.Value (CurrencySymbol, Value(..), TokenName(..))
import Ledger.Constraints qualified as Constraints

import Mlabs.Governance.Contract.Api qualified as Api
import Mlabs.Governance.Contract.Validation qualified as Validation
import Mlabs.Plutus.Contract (getEndpoint, selects)

-- do we want another error type? 
type GovernanceContract a = Contract.Contract (Maybe (Last Integer)) Api.GovernanceSchema Text a

governanceEndpoints :: CurrencySymbol -> GovernanceContract ()
governanceEndpoints csym = forever $ selects
  [ getEndpoint @Api.Deposit >>= deposit csym
  , getEndpoint @Api.Withdraw >>= withdraw csym 
  , getEndpoint @Api.ProvideRewards >>= provideRewards csym
  , getEndpoint @Api.QueryBalance >>= queryBalance csym
  ]

--- actions

deposit :: CurrencySymbol -> Api.Deposit -> GovernanceContract ()
deposit csym (Api.Deposit amnt) = do
  pkh <- pubKeyHash <$> Contract.ownPubKey
  (datum, _, oref) <- findGovernance csym

  let datum' = case AssocMap.lookup pkh datum of
        Nothing -> AssocMap.insert pkh amnt datum
        Just n  -> AssocMap.insert pkh (n+amnt) datum
      tx = sconcat [
          Constraints.mustForgeValue $ Validation.xgovValueOf (scriptCurrencySymbol $ Validation.xGovMintingPolicy csym) (coerce pkh) amnt
        , Constraints.mustPayToTheScript datum' $ Validation.govValueOf csym amnt
        , Constraints.mustSpendScriptOutput oref (Redeemer $ toData ())
        ]
      lookups = sconcat [
              Constraints.monetaryPolicy        (Validation.xGovMintingPolicy csym)
            , Constraints.otherScript           (Validation.scrValidator csym)
            , Constraints.scriptInstanceLookups (Validation.scrInstance csym)
            ]
                
  ledgerTx <- Contract.submitTxConstraintsWith @Validation.Governance lookups tx
  void $ Contract.awaitTxConfirmed $ txId ledgerTx
  Contract.logInfo @String $ printf "deposited %s GOV tokens" (show amnt)

withdraw :: CurrencySymbol -> Api.Withdraw -> GovernanceContract ()
withdraw csym (Api.Withdraw val) = do
  -- 'guard' doesn't work here
  if [(Validation.xGovCurrencySymbol csym)] == (AssocMap.keys $ getValue val) then
    Contract.throwError "Attempt to withdraw with non xGOV tokens"
  else
    pure ()

  pkh <- pubKeyHash <$> Contract.ownPubKey
  (datum, _, oref) <- findGovernance csym
  tokens <- fmap AssocMap.toList . maybe (Contract.throwError "No xGOV tokens found") pure
            . AssocMap.lookup (Validation.xGovCurrencySymbol csym) $ getValue val
  let maybedatum' :: Maybe (AssocMap.Map PubKeyHash Integer)
      maybedatum' = foldM (\mp (tn, amm) -> withdrawFromCorrect tn amm mp) datum tokens

      -- AssocMap has no "insertWith", so we have to use lookup and insert, all under foldM
      withdrawFromCorrect tn amm mp =
        case AssocMap.lookup pkh mp of
          Just n | n > amm  -> Just (AssocMap.insert depositor (n-amm) mp)
          Just n | n == amm -> Just (AssocMap.delete depositor mp)
          _                 -> Nothing
          where depositor = coerce tn
          
  datum' <- maybe (Contract.throwError "Minting policy unsound OR invalid input") pure maybedatum'
  
  let totalGov = sum $ map snd tokens
      tx = sconcat [
          Constraints.mustPayToTheScript datum' val
        , Constraints.mustPayToPubKey pkh $ Validation.govValueOf csym totalGov
        , Constraints.mustSpendScriptOutput oref (Redeemer $ toData ())
        ]
      lookups = sconcat [
              Constraints.scriptInstanceLookups (Validation.scrInstance csym)
            , Constraints.otherScript (Validation.scrValidator csym)
            ]
                
  ledgerTx <- Contract.submitTxConstraintsWith @Validation.Governance lookups tx
  void $ Contract.awaitTxConfirmed $ txId ledgerTx
  Contract.logInfo @String $ printf "withdrew %s GOV tokens" (show totalGov)

provideRewards :: CurrencySymbol -> Api.ProvideRewards -> GovernanceContract ()
provideRewards csym (Api.ProvideRewards val) = do
  (datum, _, _) <- findGovernance csym
  let -- annotates each depositor with the total percentage of GOV deposited to the contract 
      (total, props) = foldr (\(pkh, amm) (t, p) -> (amm+t, (pkh, amm%total):p)) (0, []) $ AssocMap.toList datum
      dispatch = map (\(pkh, prop) -> (pkh,Value $ fmap (round.(prop *).(%1)) <$> getValue val)) props

  let tx = foldMap (uncurry Constraints.mustPayToPubKey) dispatch
      lookups = sconcat [
              Constraints.otherScript (Validation.scrValidator csym)
            ]

  ledgerTx <- Contract.submitTxConstraintsWith @Validation.Governance lookups tx
  void $ Contract.awaitTxConfirmed $ txId ledgerTx
  Contract.logInfo @String $ printf "Provided rewards to all xGOV holders"  

queryBalance :: CurrencySymbol -> Api.QueryBalance -> GovernanceContract ()
queryBalance csym (Api.QueryBalance pkh) = do
  (datum,_,_) <- findGovernance csym
  Contract.tell . fmap Last $ AssocMap.lookup pkh datum
  
--- util

-- assumes a unique Governance. TODO: NFT 
findGovernance :: CurrencySymbol -> GovernanceContract (AssocMap.Map PubKeyHash Integer, TxOutTx, TxOutRef)
findGovernance csym = do
  utxos <- Contract.utxoAt (Validation.scrAddress csym) 
  case Map.toList utxos of
    [(oref, o)] -> case txOutDatumHash $ txOutTxOut o of
      Nothing -> Contract.throwError "unexpected out type"
      Just h  -> case Map.lookup h $ txData $ txOutTxTx o of
        Nothing        -> Contract.throwError "datum not found"
        Just (Datum e) -> case fromData e of
          Nothing -> Contract.throwError "datum has wrong type"
          Just d  -> return (d, o, oref)
    _ -> Contract.throwError "No UTxO found"
