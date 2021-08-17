{-# LANGUAGE OverloadedLists #-}

-- | Server for governance application
module Mlabs.Governance.Contract.Server (
  GovernanceContract,
  governanceEndpoints,
) where

import PlutusTx.Prelude hiding (toList)
import Prelude (String, show, uncurry)

import Control.Monad (foldM, forever, void)
import Data.Coerce (coerce)
import Data.Map qualified as Map
import Data.Semigroup (Last (..), sconcat)
import Data.Text (Text)
import Ledger.Constraints qualified as Constraints
import Mlabs.Governance.Contract.Api qualified as Api
import Mlabs.Governance.Contract.Validation (GovParams (..), GovernanceDatum (..), GovernanceRedeemer (..))
import Mlabs.Governance.Contract.Validation qualified as Validation
import Mlabs.Plutus.Contract (getEndpoint, selects)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Api (Datum (..), Redeemer (..), fromBuiltinData, toBuiltinData)
import Plutus.V1.Ledger.Crypto (PubKeyHash (..), pubKeyHash)
import Plutus.V1.Ledger.Tx (Tx (..), TxOut (..), TxOutRef, TxOutTx (..), txId)
import Plutus.V1.Ledger.Value (TokenName (..), Value (..), singleton, valueOf)
import PlutusTx.AssocMap qualified as AssocMap
import Text.Printf (printf)

-- do we want another error type?
type GovernanceContract a = Contract.Contract (Maybe (Last Integer)) Api.GovernanceSchema Text a

governanceEndpoints :: GovParams -> GovernanceContract ()
governanceEndpoints params = do
  -- FIXME temporary moved to selects to make tests work
  -- getEndpoint @Api.StartGovernance >>= startGovernance
  forever $
    selects
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
      datum' =
        GovernanceDatum
          (Validation.GRDeposit ownPkh amnt)
          (updateAmount ownPkh amnt datum.gdDepositMap)
      tx =
        sconcat
          [ Constraints.mustMintValue xGovValue
          , Constraints.mustPayToTheScript datum' $ Validation.govSingleton params.gov amnt <> traceNFT
          , Constraints.mustSpendScriptOutput oref (Redeemer . toBuiltinData $ GRDeposit ownPkh amnt)
          ]
      lookups =
        sconcat
          [ Constraints.mintingPolicy $ Validation.xGovMintingPolicy params.nft
          , Constraints.otherScript $ Validation.scrValidator params
          , Constraints.typedValidatorLookups $ Validation.scrInstance params
          , Constraints.unspentOutputs $ Map.singleton oref utxo
          ]

  ledgerTx <- Contract.submitTxConstraintsWith @Validation.Governance lookups tx
  void $ Contract.awaitTxConfirmed $ txId ledgerTx
  Contract.logInfo @String $ printf "deposited %s GOV tokens" (show amnt)
  where
    updateAmount pkh amount depositMap =
      let amount' = amount + fromMaybe 0 (AssocMap.lookup pkh depositMap)
       in AssocMap.insert pkh amount' depositMap

withdraw :: GovParams -> Api.Withdraw -> GovernanceContract ()
withdraw params (Api.Withdraw val) = do
  pkh <- pubKeyHash <$> Contract.ownPubKey
  (datum, utxo, oref) <- findGovernance params
  tokens <-
    fmap AssocMap.toList . maybe (Contract.throwError "No xGOV tokens found") pure
      . AssocMap.lookup (Validation.xGovCurrencySymbol params.nft)
      $ getValue val
  let maybemap' :: Maybe (AssocMap.Map PubKeyHash Integer)
      maybemap' = foldM (\mp (tn, amm) -> withdrawFromCorrect tn amm mp) (gdDepositMap datum) tokens

      totalPaid :: Integer
      totalPaid = sum . map snd $ tokens

      -- AssocMap has no "insertWith", so we have to use lookup and insert, all under foldM
      withdrawFromCorrect tn amm mp =
        case AssocMap.lookup pkh mp of
          Just n | n > amm -> Just (AssocMap.insert depositor (n - amm) mp)
          Just n | n == amm -> Just (AssocMap.delete depositor mp)
          _ -> Nothing
        where
          depositor = coerce tn

  datum' <-
    GovernanceDatum (Validation.GRWithdraw pkh totalPaid)
      <$> maybe (Contract.throwError "Minting policy unsound OR invalid input") pure maybemap'

  let totalGov = sum $ map snd tokens
      tx =
        sconcat
          [ -- user doesn't pay to script, but instead burns the xGOV (ensured by validators)
            Constraints.mustPayToTheScript datum' mempty
          , Constraints.mustMintValue (negate val)
          , Constraints.mustPayToPubKey pkh $ Validation.govSingleton params.gov totalGov
          , Constraints.mustSpendScriptOutput oref (Redeemer . toBuiltinData $ GRWithdraw pkh totalGov)
          ]
      lookups =
        sconcat
          [ Constraints.typedValidatorLookups $ Validation.scrInstance params
          , Constraints.otherScript $ Validation.scrValidator params
          , Constraints.mintingPolicy $ Validation.xGovMintingPolicy params.nft
          , Constraints.unspentOutputs $ Map.singleton oref utxo
          ]

  ledgerTx <- Contract.submitTxConstraintsWith @Validation.Governance lookups tx
  void $ Contract.awaitTxConfirmed $ txId ledgerTx
  Contract.logInfo @String $ printf "withdrew %s GOV tokens" (show totalGov)

provideRewards :: GovParams -> Api.ProvideRewards -> GovernanceContract ()
provideRewards params (Api.ProvideRewards val) = do
  (datum, _, _) <- findGovernance params
  let -- annotates each depositor with the total percentage of GOV deposited to the contract
      (total, props) = foldr (\(pkh, amm) (t, p) -> (amm + t, (pkh, amm % total) : p)) (0, []) $ AssocMap.toList (gdDepositMap datum)
      dispatch = map (\(pkh, prop) -> (pkh, Value $ fmap (round.(prop *).(% 1)) <$> getValue val)) props

  let tx = foldMap (uncurry Constraints.mustPayToPubKey) dispatch
      lookups =
        sconcat
          [ Constraints.otherScript $ Validation.scrValidator params
          ]

  ledgerTx <- Contract.submitTxConstraintsWith @Validation.Governance lookups tx
  void $ Contract.awaitTxConfirmed $ txId ledgerTx
  Contract.logInfo @String $ printf "Provided rewards to all xGOV holders"

queryBalance :: GovParams -> Api.QueryBalance -> GovernanceContract ()
queryBalance params (Api.QueryBalance pkh) = do
  (datum, _, _) <- findGovernance params
  Contract.tell . fmap Last $ AssocMap.lookup pkh (gdDepositMap datum)

--- util

-- assumes the Governance is parametrised by an NFT.
findGovernance :: GovParams -> GovernanceContract (Validation.GovernanceDatum, TxOutTx, TxOutRef)
findGovernance params = do
  utxos <- Contract.utxoAt $ Validation.scrAddress params
  let xs =
        [ (oref, o)
        | (oref, o) <- Map.toList utxos
        , valueOf (txOutValue $ txOutTxOut o) params.nft.acNftCurrencySymbol params.nft.acNftTokenName == 1
        ]
  case xs of
    [(oref, o)] -> case txOutDatumHash $ txOutTxOut o of
      Nothing -> Contract.throwError "unexpected out type"
      Just h -> case Map.lookup h $ txData $ txOutTxTx o of
        Nothing -> Contract.throwError "datum not found"
        Just (Datum e) -> case fromBuiltinData e of
          Nothing -> Contract.throwError "datum has wrong type"
          Just gd -> return (gd, o, oref)
    _ -> Contract.throwError "No UTxO found"
