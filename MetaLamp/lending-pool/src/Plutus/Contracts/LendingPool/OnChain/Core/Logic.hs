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
import qualified Data.ByteString                                  as BS
import qualified Data.Map                                         as Map
import           Data.Text                                        (Text, pack)
import           Data.Void                                        (Void)
import           Ext.Plutus.Ledger.Contexts                       (findOnlyOneDatumHashByValue,
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
import qualified Ledger.Typed.Scripts                             as Scripts
import           Playground.Contract
import           Plutus.Contract                                  hiding (when)
import           Plutus.Contracts.LendingPool.OnChain.Core.Script (AaveDatum (..),
                                                                   AaveRedeemer (..),
                                                                   Oracles,
                                                                   Reserve (..),
                                                                   UserConfig (..),
                                                                   UserConfigId)
import qualified Plutus.Contracts.Service.Oracle                  as Oracle
import           Plutus.V1.Ledger.Value
import qualified PlutusTx
import qualified PlutusTx.AssocMap                                as AssocMap
import           PlutusTx.Prelude                                 hiding
                                                                  (Semigroup (..),
                                                                   unless)
import           Prelude                                          (Semigroup (..))
import qualified Prelude

assertMapChange :: (Eq k, Eq a) => ((k, a) -> Bool) -> AssocMap.Map k a -> AssocMap.Map k a -> Bool
assertMapChange filterChanged old new = traceIfFalse "Unexpected datum change" $ f old == f new
  where f = filter filterChanged . AssocMap.toList

assertInsertAt :: (Eq k, Eq a) => k -> AssocMap.Map k a -> AssocMap.Map k a -> Bool
assertInsertAt key = assertMapChange $ (/= key) . fst

{-# INLINABLE pickUserConfigs #-}
pickUserConfigs :: AaveDatum -> Maybe (AssetClass, AssocMap.Map UserConfigId UserConfig)
pickUserConfigs (UserConfigsDatum stateToken configs) = Just (stateToken, configs)
pickUserConfigs _ = Nothing

{-# INLINABLE pickReserves #-}
pickReserves :: AaveDatum -> Maybe (AssetClass, AssocMap.Map AssetClass Reserve)
pickReserves (ReservesDatum stateToken configs) = Just (stateToken, configs)
pickReserves _                                  = Nothing

{-# INLINABLE pickUserCollateralFunds #-}
pickUserCollateralFunds :: AaveDatum -> Maybe (PubKeyHash, AssetClass)
pickUserCollateralFunds (UserCollateralFundsDatum user aTokenAsset) = Just (user, aTokenAsset)
pickUserCollateralFunds _ = Nothing

{-# INLINABLE totalDebtAndCollateralInLovelace #-}
totalDebtAndCollateralInLovelace ::
     PubKeyHash
  -> Oracles
  -> AssocMap.Map UserConfigId UserConfig
  -> Maybe UserConfig
totalDebtAndCollateralInLovelace actor oracles userConfigs =
  foldrM addCollateral (UserConfig 0 0) $ AssocMap.toList userConfigs
  where
    addCollateral ::
         (UserConfigId, UserConfig)
      -> UserConfig
      -> Maybe UserConfig
    addCollateral ((asset, user), userConfig) currentTotal
      | user == actor =
        (\rate -> UserConfig {
          ucCollateralizedInvestment = rate * ucCollateralizedInvestment userConfig + ucCollateralizedInvestment currentTotal,
          ucDebt = rate * ucDebt userConfig + ucDebt currentTotal }
          ) <$>
        AssocMap.lookup asset oracles
      | otherwise = Just currentTotal

{-# INLINABLE doesCollateralCoverDebt #-}
doesCollateralCoverDebt ::
     PubKeyHash
  -> Oracles
  -> AssocMap.Map UserConfigId UserConfig
  -> Bool
doesCollateralCoverDebt actor oracles userConfigs = maybe False (\UserConfig{..} -> ucDebt <= ucCollateralizedInvestment) $
  totalDebtAndCollateralInLovelace actor oracles userConfigs

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
checkNegativeReservesTransformation :: AssetClass
  -> AssocMap.Map AssetClass Reserve
  -> ScriptContext
  -> UserConfigId
  -> Bool
checkNegativeReservesTransformation stateToken reserves ctx (reserveId, _) =
      maybe False (checkReserves reserves) reservesOutputDatum
  where
    txInfo = scriptContextTxInfo ctx
    (scriptsHash, scriptsDatumHash) = ownHashes ctx
    scriptOutputs = scriptOutputsAt scriptsHash txInfo

    reservesOutputDatumHash =
      findOnlyOneDatumHashByValue (assetClassValue stateToken 1) scriptOutputs
    reservesOutputDatum ::
         Maybe (AssetClass, AssocMap.Map AssetClass Reserve)
    reservesOutputDatum =
      reservesOutputDatumHash >>= parseDatum txInfo >>= pickReserves

    remainderDatumHash = findDatumHash (Datum $ PlutusTx.toData ReserveFundsDatum) txInfo
    remainderValue = (`findValueByDatumHash` scriptOutputs) <$> remainderDatumHash

    checkReserves :: AssocMap.Map AssetClass Reserve -> (AssetClass, AssocMap.Map AssetClass Reserve) -> Bool
    checkReserves reserves (newStateToken, newReserves) =
      newStateToken == stateToken &&
      assertInsertAt reserveId reserves newReserves &&
      maybe
        False
        checkReserveState
        ((,,) <$> remainderValue <*> AssocMap.lookup reserveId reserves <*> AssocMap.lookup reserveId newReserves)
    checkReserveState :: (Value, Reserve, Reserve) -> Bool
    checkReserveState (value, oldState, newState) =
      let fundsAmount = rAmount newState
      in  assetClassValueOf value reserveId == fundsAmount && fundsAmount >= 0 && checkReservesConsistency oldState newState

{-# INLINABLE checkPositiveReservesTransformation #-}
checkPositiveReservesTransformation :: AssetClass
  -> AssocMap.Map AssetClass Reserve
  -> ScriptContext
  -> UserConfigId
  -> Bool
checkPositiveReservesTransformation stateToken reserves ctx (reserveId, _) = maybe False (checkReserves reserves) reservesOutputDatum
  where
    txInfo = scriptContextTxInfo ctx
    (scriptsHash, scriptsDatumHash) = ownHashes ctx
    scriptOutputs = scriptOutputsAt scriptsHash txInfo

    reservesOutputDatumHash =
      findOnlyOneDatumHashByValue (assetClassValue stateToken 1) scriptOutputs
    reservesOutputDatum ::
         Maybe (AssetClass, AssocMap.Map AssetClass Reserve)
    reservesOutputDatum =
      reservesOutputDatumHash >>= parseDatum txInfo >>= pickReserves

    investmentDatumHash = findDatumHash (Datum $ PlutusTx.toData ReserveFundsDatum) txInfo
    investmentValue = (`findValueByDatumHash` scriptOutputs) <$> investmentDatumHash

    checkReserves :: AssocMap.Map AssetClass Reserve -> (AssetClass, AssocMap.Map AssetClass Reserve) -> Bool
    checkReserves reserves (newStateToken, newReserves) =
      newStateToken == stateToken &&
      assertInsertAt reserveId reserves newReserves &&
      maybe
        False
        checkReserveState
        ((,,) <$> investmentValue <*> AssocMap.lookup reserveId reserves <*> AssocMap.lookup reserveId newReserves)
    checkReserveState :: (Value, Reserve, Reserve) -> Bool
    checkReserveState (value, oldState, newState) =
      let fundsChange = rAmount newState - rAmount oldState
      in  assetClassValueOf value reserveId == fundsChange && fundsChange > 0 && checkReservesConsistency oldState newState

{-# INLINABLE checkReservesConsistency #-}
checkReservesConsistency :: Reserve -> Reserve -> Bool
checkReservesConsistency oldState newState =
  rCurrency oldState == rCurrency newState &&
  rAToken oldState == rAToken newState &&
  rLiquidityIndex oldState == rLiquidityIndex newState &&
  rCurrentStableBorrowRate oldState == rCurrentStableBorrowRate newState &&
  Oracle.fromTuple (rTrustedOracle oldState) == Oracle.fromTuple (rTrustedOracle newState)
