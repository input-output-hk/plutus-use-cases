{-# LANGUAGE OverloadedLists #-}

-- | Server for governance application
module Mlabs.Governance.Contract.Server (
  GovernanceContract,
  governanceEndpoints,
) where

import PlutusTx.Prelude hiding (toList, uncurry)
import Prelude (String, show, uncurry)

import Control.Lens ((^.), (^?))
import Control.Monad (forever, void)
import Data.List.Extra (maximumOn)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Semigroup (Last (..), sconcat)
import Data.Text (Text)
import Ledger.Constraints qualified as Constraints
import Ledger.Crypto (PubKeyHash (..), pubKeyHash)
import Ledger.Tx (ChainIndexTxOut, Tx (..), TxOut (..), TxOutRef, TxOutTx (..), ciTxOutDatum, ciTxOutValue, fromTxOut, toTxOut, txId, txOutPubKey)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Api (Datum (..), Redeemer (..), fromBuiltinData, toBuiltinData)
import Plutus.V1.Ledger.Value (Value (..), valueOf)
import Text.Printf (printf)

import GHC.Base (Maybe (Nothing))
import Mlabs.Governance.Contract.Api qualified as Api
import Mlabs.Governance.Contract.Validation (AssetClassGov (..), GovernanceDatum (..), GovernanceRedeemer (..))
import Mlabs.Governance.Contract.Validation qualified as Validation
import Mlabs.Plutus.Contract (getEndpoint, selectForever)
import PlutusTx.Prelude (sequenceA)

--import GHC.Base (Applicative(pure))

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
              , Constraints.mustPayToTheScript datum $ Validation.govSingleton gov amnt <> (utxo ^. ciTxOutValue)
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
              scriptBalance = utxo ^. ciTxOutValue
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
      (total, props) = foldr (\(pkh, amm) (t, p) -> (amm + t, (pkh, amm % total) : p)) (0, mempty) depositMap

      dispatch =
        map
          ( \(pkh, prop) ->
              case pkh of
                Just pkh' -> Just (pkh', Value $ fmap (round.(prop *).(% 1)) <$> getValue val)
                Nothing -> Nothing
          )
          props

  let aux = \case
        Just x -> Just $ uncurry Constraints.mustPayToPubKey x
        Nothing -> Nothing
  tx <- maybe err pure $ foldMap aux dispatch

  let lookups =
        sconcat
          [ Constraints.otherScript $ Validation.govValidator gov
          ]

  ledgerTx <- Contract.submitTxConstraintsWith @Validation.Governance lookups tx
  void $ Contract.awaitTxConfirmed $ txId ledgerTx
  Contract.logInfo @String $ printf "Provided rewards to all xGOV holders"
  where
    err = Contract.throwError "Could not find PublicKeyHash."

    govOf v = valueOf v (acGovCurrencySymbol gov) (acGovTokenName gov)
    getPkh (_, o) = (,) ((txOutPubKey . toTxOut) o) (govOf . txOutValue . toTxOut $ o)
    depositMapC = do
      utxos <- fmap Map.toList . Contract.utxosAt $ Validation.govAddress gov
      pure $ getPkh <$> utxos

queryBalance :: AssetClassGov -> Api.QueryBalance -> GovernanceContract ()
queryBalance gov (Api.QueryBalance pkh) = do
  amm <- maybe 0 foo <$> findGovernance pkh gov
  Contract.tell . Just $ Last amm
  where
    foo (_, tx, _) = govOf $ tx ^. ciTxOutValue
    govOf v = valueOf v (acGovCurrencySymbol gov) (acGovTokenName gov)

--- util

-- looks for governance, returns one with the biggest GOV value attached to it, if it exists
findGovernance ::
  PubKeyHash ->
  AssetClassGov ->
  GovernanceContract (Maybe (Validation.GovernanceDatum, ChainIndexTxOut, TxOutRef))
findGovernance pkh gov@AssetClassGov {..} = do
  utxos <- Contract.utxosAt $ Validation.govAddress gov
  case Map.toList utxos >>= foo of
    [] -> pure Nothing
    xs -> pure . Just $ maximumOn getVal xs
  where
    govOf v = valueOf v acGovCurrencySymbol acGovTokenName

    getVal (_, tx, _) = govOf $ tx ^. ciTxOutValue
    foo (oref, o) = case o ^? ciTxOutDatum of
      Just (Right (Datum e)) -> case fromBuiltinData e of
        Just gd | gd == pkh -> [(GovernanceDatum gd acGovCurrencySymbol, o, oref)]
        _ -> mempty
      _ -> mempty
