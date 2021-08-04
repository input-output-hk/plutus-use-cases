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
import Plutus.V1.Ledger.Value (Value(..), TokenName(..), valueOf, singleton)
import Ledger.Constraints qualified as Constraints

import Mlabs.Governance.Contract.Api qualified as Api
import Mlabs.Governance.Contract.Api (AssetClassNft(..), AssetClassGov(..))
import Mlabs.Governance.Contract.Validation qualified as Validation
import Mlabs.Governance.Contract.Validation (GovernanceDatum(..))
import Mlabs.Plutus.Contract (getEndpoint, selects)

-- do we want another error type? 
type GovernanceContract a = Contract.Contract (Maybe (Last Integer)) Api.GovernanceSchema Text a

governanceEndpoints :: AssetClassNft -> AssetClassGov -> GovernanceContract ()
governanceEndpoints nft gov = do
  -- some nft gov duplication here, probably have to refactor all
  -- of the Api types to hold nft and gov themselves. TBD (do we want them as params or not?)
  getEndpoint @Api.StartGovernance >>= startGovernance 
  forever $ selects
    [ getEndpoint @Api.Deposit >>= deposit nft gov
    , getEndpoint @Api.Withdraw >>= withdraw nft gov
    , getEndpoint @Api.ProvideRewards >>= provideRewards nft gov
    , getEndpoint @Api.QueryBalance >>= queryBalance nft gov
    ]

--- actions

startGovernance :: Api.StartGovernance -> GovernanceContract ()
startGovernance (Api.StartGovernance nft gov) = do
  let d = GovernanceDatum nft gov AssocMap.empty
      v = singleton (acNftCurrencySymbol nft) (acNftTokenName nft) 1
      tx = Constraints.mustPayToTheScript d v
  ledgerTx <- Contract.submitTxConstraints Validation.scrInstance tx
  void $ Contract.awaitTxConfirmed $ txId ledgerTx
  Contract.logInfo @String $ printf "Started governance for nft token %s, gov token %s" (show nft) (show gov)

deposit :: AssetClassNft -> AssetClassGov -> Api.Deposit -> GovernanceContract ()
deposit nft gov (Api.Deposit amnt) = do
  pkh <- pubKeyHash <$> Contract.ownPubKey
  (datum, _, oref) <- findGovernance nft gov

  let datum' = GovernanceDatum (gdNft datum) (gdGov datum) $
        case AssocMap.lookup pkh (gdDepositMap datum) of
          Nothing -> AssocMap.insert pkh amnt (gdDepositMap datum)
          Just n  -> AssocMap.insert pkh (n+amnt) (gdDepositMap datum)
      tx = sconcat [
          Constraints.mustForgeValue $ Validation.xgovValueOf (scriptCurrencySymbol
            $ Validation.xGovMintingPolicy gov) (coerce pkh) amnt
        , Constraints.mustPayToTheScript datum' $ Validation.govValueOf gov amnt
        , Constraints.mustSpendScriptOutput oref (Redeemer $ toData ())
        ]
      lookups = sconcat [
              Constraints.monetaryPolicy        (Validation.xGovMintingPolicy gov)
            , Constraints.otherScript           Validation.scrValidator
            , Constraints.scriptInstanceLookups Validation.scrInstance
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
            . AssocMap.lookup (acGovCurrencySymbol gov) $ getValue val
  let maybedatum' :: Maybe (AssocMap.Map PubKeyHash Integer)
      maybedatum' = foldM (\mp (tn, amm) -> withdrawFromCorrect tn amm mp) (gdDepositMap datum) tokens

      -- AssocMap has no "insertWith", so we have to use lookup and insert, all under foldM
      withdrawFromCorrect tn amm mp =
        case AssocMap.lookup pkh mp of
          Just n | n > amm  -> Just (AssocMap.insert depositor (n-amm) mp)
          Just n | n == amm -> Just (AssocMap.delete depositor mp)
          _                 -> Nothing
          where depositor = coerce tn
          
  datum' <- GovernanceDatum (gdNft datum) (gdGov datum)
              <$> maybe (Contract.throwError "Minting policy unsound OR invalid input") pure maybedatum'
  
  let totalGov = sum $ map snd tokens
      tx = sconcat [
          Constraints.mustPayToTheScript datum' val
        , Constraints.mustPayToPubKey pkh $ Validation.govValueOf gov totalGov
        , Constraints.mustSpendScriptOutput oref (Redeemer $ toData ())
        ]
      lookups = sconcat [
              Constraints.scriptInstanceLookups Validation.scrInstance
            , Constraints.otherScript Validation.scrValidator
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
              Constraints.otherScript Validation.scrValidator
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
  utxos <- Contract.utxoAt Validation.scrAddress
  let xs = [ (oref, o)
           | (oref, o) <- Map.toList utxos
           , valueOf (txOutValue $ txOutTxOut o) (acNftCurrencySymbol nft) (acNftTokenName nft) == 1
           ]
  case xs of
    [(oref, o)] -> case txOutDatumHash $ txOutTxOut o of
      Nothing -> Contract.throwError "unexpected out type"
      Just h  -> case Map.lookup h $ txData $ txOutTxTx o of
        Nothing        -> Contract.throwError "datum not found"
        Just (Datum e) -> case fromData e of
          Nothing -> Contract.throwError "datum has wrong type"
          Just gd@GovernanceDatum{..}
            | acNftCurrencySymbol gdNft == acNftCurrencySymbol nft &&
              acNftTokenName gdNft == acNftTokenName nft &&
              acGovCurrencySymbol gdGov == acGovCurrencySymbol gov &&
              acGovTokenName gdGov == acGovTokenName gov -> return (gd, o, oref)
            | otherwise                                  -> Contract.throwError "Governance tokens mismatch"
    _ -> Contract.throwError "No UTxO found"
