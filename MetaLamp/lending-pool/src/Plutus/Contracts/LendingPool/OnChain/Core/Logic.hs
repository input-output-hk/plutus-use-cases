{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fobject-code #-}

module Plutus.Contracts.LendingPool.OnChain.Core.Logic where

import           Control.Lens                                     ((^?))
import qualified Control.Lens                                     as Lens
import           Control.Monad                                    hiding (fmap)
import qualified Data.Bifunctor                                   as Bifunctor
import qualified Data.ByteString                                  as BS
import qualified Data.Map                                         as Map
import           Data.Text                                        (Text, pack)
import           Data.Void                                        (Void)
import           Ext.Plutus.Ledger.Contexts                       (findOnlyOneDatumByValue,
                                                                   findValueByDatum,
                                                                   findValueByDatumHash,
                                                                   parseDatum,
                                                                   scriptInputsAt,
                                                                   valueSpentFrom)
import           Ledger                                           hiding
                                                                  (singleton)
import           Ledger.Constraints                               as Constraints
import           Ledger.Constraints.OnChain                       as Constraints
import           Ledger.Constraints.TxConstraints                 as Constraints
import qualified Ledger.Scripts                                   as UntypedScripts
import           Ledger.TimeSlot                                  (posixTimeRangeToSlotRange)
import qualified Ledger.Typed.Scripts                             as Scripts
import           Playground.Contract
import           Plutus.Abstract.IncentivizedAmount               (IncentivizedAmount (..))
import           Plutus.Contract                                  hiding (when)
import           Plutus.Contracts.LendingPool.OnChain.Core.Script (AaveDatum (..),
                                                                   AaveRedeemer (..),
                                                                   AaveState (..),
                                                                   Oracles,
                                                                   Reserve (..),
                                                                   UserConfig (..),
                                                                   UserConfigId)
import qualified Plutus.Contracts.Service.Oracle                  as Oracle
import qualified Plutus.V1.Ledger.Interval                        as Interval
import           Plutus.V1.Ledger.Value
import qualified PlutusTx
import qualified PlutusTx.AssocMap                                as AssocMap
import qualified PlutusTx.Builtins                                as Builtins
import           PlutusTx.Prelude                                 hiding
                                                                  (Semigroup (..),
                                                                   unless)
import           Prelude                                          (Semigroup (..))
import qualified Prelude

assertMapChange :: (Eq k, Eq a) => ((k, a) -> Bool) -> AssocMap.Map k a -> AssocMap.Map k a -> Either Builtins.String ()
assertMapChange filterChanged old new = unless (f old == f new) (throwError "Unexpected datum change")
  where f = filter filterChanged . AssocMap.toList

assertInsertAt :: (Eq k, Eq a) => k -> AssocMap.Map k a -> AssocMap.Map k a -> Either Builtins.String ()
assertInsertAt key = assertMapChange $ (/= key) . fst

{-# INLINABLE pickUserConfigs #-}
pickUserConfigs :: AaveDatum -> Maybe (AaveState, AssocMap.Map UserConfigId UserConfig)
pickUserConfigs (UserConfigsDatum state configs) = Just (state, configs)
pickUserConfigs _                                = Nothing

{-# INLINABLE pickReserves #-}
pickReserves :: AaveDatum -> Maybe (AaveState, AssocMap.Map AssetClass Reserve)
pickReserves (ReservesDatum state configs) = Just (state, configs)
pickReserves _                             = Nothing

{-# INLINABLE pickUserCollateralFunds #-}
pickUserCollateralFunds :: AaveDatum -> Maybe (PubKeyHash, AssetClass)
pickUserCollateralFunds (UserCollateralFundsDatum user aTokenAsset) = Just (user, aTokenAsset)
pickUserCollateralFunds _ = Nothing

toBool :: Either Builtins.String () -> Bool
toBool (Left m)  = traceError m
toBool (Right m) = True

fromBool :: Builtins.String -> Bool -> Either Builtins.String ()
fromBool _ True  = pure ()
fromBool e False = Left e

toBoolPrefixed :: Builtins.String -> Either Builtins.String () -> Bool
toBoolPrefixed prefix = toBool . Bifunctor.first (Builtins.appendString prefix)

assertValidCurrentSlot :: ScriptContext -> Slot -> Either Builtins.String ()
assertValidCurrentSlot ctx slot = fromBool "Invalid current slot value" $
  Interval.LowerBound (Interval.Finite slot) True == Interval.ivFrom (posixTimeRangeToSlotRange . txInfoValidRange . scriptContextTxInfo $ ctx)

findUserConfigs :: ScriptContext -> AaveState -> Either Builtins.String (AssocMap.Map UserConfigId UserConfig)
findUserConfigs ctx state@AaveState{..} = do
  let txInfo = scriptContextTxInfo ctx
  (newState, newUserConfigs) <- maybe (throwError "User configs not found") pure $
    findOnlyOneDatumByValue ctx (assetClassValue asUserConfigs 1) >>= pickUserConfigs
  unless (newState == state) $ throwError "Invalid state address change"
  pure newUserConfigs

findReserves :: ScriptContext -> AaveState -> Either Builtins.String (AssocMap.Map AssetClass Reserve)
findReserves ctx state@AaveState{..} = do
  let txInfo = scriptContextTxInfo ctx
  (newState, newReserves) <- maybe (throwError "Reserves not found") pure $
    findOnlyOneDatumByValue ctx (assetClassValue asReserves 1) >>= pickReserves
  unless (newState == state) $ throwError "Invalid state address change"
  pure newReserves

{-# INLINABLE doesCollateralCoverDebt #-}
doesCollateralCoverDebt ::
     PubKeyHash
  -> Oracles
  -> AssocMap.Map UserConfigId UserConfig
  -> Bool
doesCollateralCoverDebt actor oracles userConfigs = Just True == do
  let byUser = filter (\((_, pkh), _) -> pkh == actor) . AssocMap.toList $ userConfigs
  debt <- totalInLovelace oracles $ fmap (\((asset, _), config) -> (asset, iaAmount . ucDebt $ config)) byUser
  investement <- totalInLovelace oracles $ fmap (\((asset, _), config) -> (asset, iaAmount . ucCollateralizedInvestment $ config)) byUser
  pure $ debt <= investement

{-# INLINABLE totalInLovelace #-}
totalInLovelace :: Oracles -> [(AssetClass, Rational)] -> Maybe Rational
totalInLovelace oracles = foldrM reducer (fromInteger 0)
  where
    reducer (asset, amount) acc = (\rate -> fromInteger rate * amount + acc) <$> AssocMap.lookup asset oracles

{-# INLINABLE areOraclesTrusted #-}
areOraclesTrusted :: [(CurrencySymbol, PubKeyHash, Integer, AssetClass)]
  -> AssocMap.Map AssetClass Reserve
  -> Bool
areOraclesTrusted oracles reserves = all checkOracle oracles
  where
    checkOracle o = let oracle = Oracle.fromTuple o in
       Just oracle == (Oracle.fromTuple . rTrustedOracle <$> AssocMap.lookup (Oracle.oAsset oracle) reserves)

{-# INLINABLE checkNegativeFundsTransformation #-}
checkNegativeFundsTransformation :: ScriptContext -> AssetClass -> PubKeyHash -> Bool
checkNegativeFundsTransformation ctx asset actor = isValidFundsChange
  where
    txInfo = scriptContextTxInfo ctx
    (scriptsHash, scriptsDatumHash) = ownHashes ctx
    scriptOutputs = scriptOutputsAt scriptsHash txInfo

    scriptSpentValue = findValueByDatumHash scriptsDatumHash $ scriptInputsAt scriptsHash txInfo
    scriptRemainderValue = findValueByDatumHash scriptsDatumHash scriptOutputs
    actorSpentValue = valueSpentFrom txInfo actor
    actorRemainderValue = valuePaidTo txInfo actor

    isValidFundsChange :: Bool
    isValidFundsChange =
      let paidAmout = assetClassValueOf actorRemainderValue asset - assetClassValueOf actorSpentValue asset
          fundsChange = assetClassValueOf scriptSpentValue asset - assetClassValueOf scriptRemainderValue asset
       in fundsChange == paidAmout && fundsChange > 0 && paidAmout > 0

{-# INLINABLE checkNegativeReservesTransformation #-}
checkNegativeReservesTransformation :: AaveState
  -> AssocMap.Map AssetClass Reserve
  -> ScriptContext
  -> UserConfigId
  -> Bool
checkNegativeReservesTransformation state@AaveState{..} reserves ctx (reserveId, _) =
  toBool $ do
    newReserves <- findReserves ctx state
    assertInsertAt reserveId reserves newReserves
    remainderValue <- maybe (throwError "Remainder not found") pure . findValueByDatum ctx $ ReserveFundsDatum
    oldState <- maybe (throwError "Reserve not found") pure . AssocMap.lookup reserveId $ reserves
    newState <- maybe (throwError "Reserve not found") pure . AssocMap.lookup reserveId $ newReserves
    let fundsAmount = rAmount newState
    unless
      (assetClassValueOf remainderValue reserveId == fundsAmount && fundsAmount >= 0 && checkReservesConsistency oldState newState)
      (throwError "")

{-# INLINABLE checkPositiveReservesTransformation #-}
checkPositiveReservesTransformation :: AaveState
  -> AssocMap.Map AssetClass Reserve
  -> ScriptContext
  -> UserConfigId
  -> Bool
checkPositiveReservesTransformation state@AaveState{..}reserves ctx (reserveId, _) =
  toBool $ do
    newReserves <- findReserves ctx state
    assertInsertAt reserveId reserves newReserves
    investmentValue <- maybe (throwError "Investment not found") pure . findValueByDatum ctx $ ReserveFundsDatum
    oldState <- maybe (throwError "Reserve not found") pure . AssocMap.lookup reserveId $ reserves
    newState <- maybe (throwError "Reserve not found") pure . AssocMap.lookup reserveId $ newReserves
    let fundsChange = rAmount newState - rAmount oldState
    unless
      (assetClassValueOf investmentValue reserveId == fundsChange && fundsChange > 0 && checkReservesConsistency oldState newState)
      (throwError "")

{-# INLINABLE checkReservesConsistency #-}
checkReservesConsistency :: Reserve -> Reserve -> Bool
checkReservesConsistency oldState newState =
  rCurrency oldState == rCurrency newState &&
  rAToken oldState == rAToken newState &&
  rLiquidityIndex oldState == rLiquidityIndex newState &&
  rCurrentStableBorrowRate oldState == rCurrentStableBorrowRate newState &&
  Oracle.fromTuple (rTrustedOracle oldState) == Oracle.fromTuple (rTrustedOracle newState)
