{-# LANGUAGE OverloadedLists #-}

-- | Server for governance application
module Mlabs.Governance.Contract.Server (
  GovernanceContract,
  governanceEndpoints,
) where

import PlutusTx.Prelude hiding (toList, uncurry)
import Prelude (String, show, uncurry)

import Control.Monad (forever, void)
import Data.List.Extra (maximumOn)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Semigroup (Last (..), sconcat)
import Data.Text (Text)
import Ledger.Constraints qualified as Constraints
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Api (Datum (..), Redeemer (..), fromBuiltinData, toBuiltinData)
import Ledger.Crypto (PubKeyHash (..), pubKeyHash)
import Ledger.Tx (Tx (..), TxOut (..), TxOutRef, TxOutTx (..), txId)
import Plutus.V1.Ledger.Value (Value (..), valueOf)
import Text.Printf (printf)

import Mlabs.Governance.Contract.Api qualified as Api
import Mlabs.Governance.Contract.Validation (AssetClassGov (..), GovernanceDatum (..), GovernanceRedeemer (..))
import Mlabs.Governance.Contract.Validation qualified as Validation
import Mlabs.Plutus.Contract (selectForever, getEndpoint)

type GovernanceContract a = Contract.Contract (Maybe (Last Integer)) Api.GovernanceSchema Text a

governanceEndpoints :: AssetClassGov -> GovernanceContract ()
governanceEndpoints gov =
  selectForever
    [ getEndpoint @Api.Deposit $ deposit gov
    , getEndpoint @Api.Withdraw $ withdraw gov
    , getEndpoint @Api.ProvideRewards $ provideRewards gov
    , getEndpoint @Api.QueryBalance $ queryBalance gov
    ]

--- actions

deposit :: AssetClassGov -> Api.Deposit -> GovernanceContract ()
deposit gov (Api.Deposit amnt) = do
  ownPkh <- pubKeyHash <$> Contract.ownPubKey
  g <- findGovernance ownPkh gov
  let (tx, lookups) = case g of
        Just (datum, utxo, oref) ->
          ( sconcat
              [ Constraints.mustMintValue xGovValue
              , Constraints.mustPayToTheScript datum $ Validation.govSingleton gov amnt <> txOutValue (txOutTxOut utxo)
              , Constraints.mustSpendScriptOutput oref (Redeemer . toBuiltinData $ GRDeposit amnt)
              ]
          , sconcat
              [ Constraints.mintingPolicy $ Validation.xGovMintingPolicy gov
              , Constraints.otherScript $ Validation.govValidator gov
              , Constraints.typedValidatorLookups $ Validation.govInstance gov
              , Constraints.unspentOutputs $ Map.singleton oref utxo
              ]
          )
        Nothing ->
          let datum = GovernanceDatum ownPkh $ Validation.xGovCurrencySymbol gov
           in ( sconcat
                  [ Constraints.mustMintValue xGovValue
                  , Constraints.mustPayToTheScript datum $ Validation.govSingleton gov amnt
                  ]
              , sconcat
                  [ Constraints.mintingPolicy $ Validation.xGovMintingPolicy gov
                  , Constraints.otherScript $ Validation.govValidator gov
                  , Constraints.typedValidatorLookups $ Validation.govInstance gov
                  ]
              )

      xGovValue = Validation.xgovSingleton gov ownPkh amnt

  ledgerTx <- Contract.submitTxConstraintsWith @Validation.Governance lookups tx
  void $ Contract.awaitTxConfirmed $ txId ledgerTx
  Contract.logInfo @String $ printf "deposited %s GOV tokens" (show amnt)

withdraw :: AssetClassGov -> Api.Withdraw -> GovernanceContract ()
withdraw gov (Api.Withdraw assets) = do
  ownPkh <- pubKeyHash <$> Contract.ownPubKey
  let trav f ~(x NE.:| xs) = (NE.:|) <$> f x <*> traverse f xs
  -- for some reason NonEmpty doesn't have a Traversible instance in scope
  (tx, lookups) <- fmap sconcat . flip trav (NE.fromList assets) $ \ac -> do
    g <- findGovernance (fst ac) gov
    case g of
      Nothing -> Contract.throwError "not found governance to withdraw from"
      Just (datum, utxo, oref) ->
        pure $
          let valxGov = Validation.xgovSingleton gov (fst ac) (snd ac)
              valGov = Validation.govSingleton gov (snd ac)
              scriptBalance = txOutValue $ txOutTxOut utxo
           in ( sconcat
                  [ Constraints.mustPayToTheScript datum $ scriptBalance - valGov
                  , Constraints.mustPayToPubKey ownPkh valGov
                  , Constraints.mustMintValue (negate valxGov)
                  , Constraints.mustSpendScriptOutput oref (Redeemer . toBuiltinData . GRWithdraw $ snd ac)
                  ]
              , sconcat
                  [ Constraints.typedValidatorLookups $ Validation.govInstance gov
                  , Constraints.otherScript $ Validation.govValidator gov
                  , Constraints.mintingPolicy $ Validation.xGovMintingPolicy gov
                  , Constraints.unspentOutputs $ Map.singleton oref utxo
                  ]
              )

  ledgerTx <- Contract.submitTxConstraintsWith @Validation.Governance lookups tx
  void $ Contract.awaitTxConfirmed $ txId ledgerTx
  Contract.logInfo @String $ printf "withdrew %s GOV tokens" (show . sum $ map snd assets)

-- TODO fix (works but transaction sizes are HUGE)
provideRewards :: AssetClassGov -> Api.ProvideRewards -> GovernanceContract ()
provideRewards gov (Api.ProvideRewards val) = do
  depositMap <- depositMapC
  let -- annotates each depositor with the total percentage of GOV deposited to the contract
      (total, props) = foldr (\(pkh, amm) (t, p) -> (amm + t, (pkh, amm % total) : p)) (0, []) depositMap
      dispatch = map (\(pkh, prop) -> (pkh, Value $ fmap (round.(prop *).(% 1)) <$> getValue val)) props

  let tx = foldMap (uncurry Constraints.mustPayToPubKey) dispatch
      lookups =
        sconcat
          [ Constraints.otherScript $ Validation.govValidator gov
          ]

  ledgerTx <- Contract.submitTxConstraintsWith @Validation.Governance lookups tx
  void $ Contract.awaitTxConfirmed $ txId ledgerTx
  Contract.logInfo @String $ printf "Provided rewards to all xGOV holders"
  where
    govOf v = valueOf v (acGovCurrencySymbol gov) (acGovTokenName gov)

    getPkh (_, o) = case txOutDatumHash $ txOutTxOut o of
      Nothing -> []
      Just h -> case Map.lookup h $ txData $ txOutTxTx o of
        Nothing -> []
        Just (Datum e) -> case fromBuiltinData e of
          Nothing -> []
          Just gd -> [(gdPubKeyHash gd, govOf . txOutValue . txOutTxOut $ o)]

    depositMapC = do
      utxos <- fmap Map.toList . Contract.utxoAt $ Validation.govAddress gov
      pure $ utxos >>= getPkh

queryBalance :: AssetClassGov -> Api.QueryBalance -> GovernanceContract ()
queryBalance gov (Api.QueryBalance pkh) = do
  amm <- maybe 0 foo <$> findGovernance pkh gov
  Contract.tell . Just $ Last amm
  where
    foo (_, tx, _) = govOf . txOutValue $ txOutTxOut tx
    govOf v = valueOf v (acGovCurrencySymbol gov) (acGovTokenName gov)

--- util

-- looks for governance, returns one with the biggest GOV value attached to it, if it exists
findGovernance :: PubKeyHash -> AssetClassGov -> GovernanceContract (Maybe (Validation.GovernanceDatum, TxOutTx, TxOutRef))
findGovernance pkh gov@AssetClassGov {..} = do
  utxos <- Contract.utxoAt $ Validation.govAddress gov
  case Map.toList utxos >>= foo of
    [] -> pure Nothing
    xs -> pure . Just $ maximumOn getVal xs
  where
    govOf v = valueOf v acGovCurrencySymbol acGovTokenName
    getVal (_, tx, _) = govOf . txOutValue $ txOutTxOut tx

    foo (oref, o) = case txOutDatumHash $ txOutTxOut o of
      Nothing -> []
      Just h -> case Map.lookup h $ txData $ txOutTxTx o of
        Nothing -> []
        Just (Datum e) -> case fromBuiltinData e of
          Just gd | gdPubKeyHash gd == pkh -> [(gd, o, oref)]
          _ -> []
