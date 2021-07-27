{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fobject-code #-}

module Plutus.Contracts.LendingPool.OnChain.Core.Validator (Aave(..), aaveInstance) where

import           Control.Lens                                     (over, (^?))
import qualified Control.Lens                                     as Lens
import           Control.Monad                                    hiding (fmap)
import qualified Data.ByteString                                  as BS
import qualified Data.Map                                         as Map
import           Data.Text                                        (Text, pack)
import           Data.Void                                        (Void)
import           Ext.Plutus.Ledger.Contexts                       (findOnlyOneDatumByValue,
                                                                   findOnlyOneDatumHashByValue,
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
import           Plutus.Abstract.IncentivizedAmount               (IncentivizedAmount (..),
                                                                   accrue)
import           Plutus.Contract                                  hiding (when)
import           Plutus.Contracts.LendingPool.OnChain.Core.Logic  (areOraclesTrusted,
                                                                   assertInsertAt,
                                                                   assertValidCurrentSlot,
                                                                   checkNegativeFundsTransformation,
                                                                   checkNegativeReservesTransformation,
                                                                   checkPositiveReservesTransformation,
                                                                   doesCollateralCoverDebt,
                                                                   findReserves,
                                                                   findUserConfigs,
                                                                   pickReserves,
                                                                   pickUserCollateralFunds,
                                                                   pickUserConfigs,
                                                                   toBool,
                                                                   toBoolPrefixed)
import           Plutus.Contracts.LendingPool.OnChain.Core.Script (Aave (..),
                                                                   AaveDatum (..),
                                                                   AaveRedeemer (..),
                                                                   AaveScript,
                                                                   AaveState (..),
                                                                   Reserve (..),
                                                                   UserConfig (..),
                                                                   UserConfigId,
                                                                   _ucCollateralizedInvestment)
import           Plutus.Contracts.LendingPool.Shared              (updateConfigAmounts)
import qualified Plutus.Contracts.Service.Oracle                  as Oracle
import           Plutus.V1.Ledger.Value
import qualified PlutusTx
import qualified PlutusTx.AssocMap                                as AssocMap
import qualified PlutusTx.Builtins                                as Builtins
import           PlutusTx.Prelude                                 hiding
                                                                  (Semigroup (..),
                                                                   unless)
import           PlutusTx.Ratio                                   as Ratio
import           Prelude                                          (Semigroup (..))
import qualified Prelude

aaveInstance :: Aave -> Scripts.TypedValidator AaveScript
aaveInstance aave = Scripts.mkTypedValidator @AaveScript
    ($$(PlutusTx.compile [|| makeAaveValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode aave)
     $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @AaveDatum @AaveRedeemer

{-# INLINABLE makeAaveValidator #-}
-- Main validator
-- Each state field must have one or more associated actions(Redeemer types),
-- produced on state update, which are then validated here
-- TODO: combine state(datums) in order to ensure that there is only one redeemer for all inputs
makeAaveValidator :: Aave
                   -> AaveDatum
                   -> AaveRedeemer
                   -> ScriptContext
                   -> Bool
makeAaveValidator aave datum StartRedeemer ctx = trace "StartRedeemer" $ validateStart aave datum ctx
makeAaveValidator aave datum (DepositRedeemer userConfigId) ctx  = trace "DepositRedeemer" $ validateDeposit aave datum ctx userConfigId
makeAaveValidator aave datum (WithdrawRedeemer userConfigId) ctx = trace "WithdrawRedeemer" $ validateWithdraw aave datum ctx userConfigId
makeAaveValidator aave datum (BorrowRedeemer userConfigId oracles slot) ctx   = trace "BorrowRedeemer" $ validateBorrow aave datum ctx userConfigId oracles slot
makeAaveValidator aave datum (RepayRedeemer userConfigId slot) ctx    = trace "RepayRedeemer" $ validateRepay aave datum ctx userConfigId slot
makeAaveValidator aave datum (ProvideCollateralRedeemer userConfigId) ctx    = trace "ProvideCollateralRedeemer" $ validateProvideCollateral aave datum ctx userConfigId
makeAaveValidator aave datum (RevokeCollateralRedeemer userConfigId aTokenAsset oracles slot) ctx    = trace "RevokeCollateralRedeemer" $ validateRevokeCollateral aave datum ctx userConfigId aTokenAsset oracles slot

{-# INLINABLE validateStart #-}
validateStart :: Aave -> AaveDatum -> ScriptContext -> Bool
validateStart aave (LendingPoolDatum operator) ctx =
  traceIfFalse "validateStart: Lending Pool Datum management is not authorized by operator"
    (isSignedByOperator && hasOutputWithSameOperator)
  where
    txInfo = scriptContextTxInfo ctx
    isSignedByOperator = txSignedBy txInfo operator

    (scriptsHash, scriptsDatumHash) = ownHashes ctx
    hasOutputWithSameOperator = case scriptOutputsAt scriptsHash txInfo of
      outs -> isJust $ AssocMap.lookup scriptsDatumHash $ AssocMap.fromList outs
validateStart aave _ ctx = trace "validateStart: Lending Pool Datum management is not allowed" False

{-# INLINABLE validateDeposit #-}
validateDeposit :: Aave -> AaveDatum -> ScriptContext -> UserConfigId -> Bool
validateDeposit aave (UserConfigsDatum state@AaveState{..} userConfigs) ctx userConfigId =
  toBoolPrefixed "validateDeposit: " $ do
    newUserConfigs <- findUserConfigs ctx state
    assertInsertAt userConfigId userConfigs newUserConfigs
    let oldState = AssocMap.lookup userConfigId userConfigs
    newState <- maybe (throwError "User config not found") pure (AssocMap.lookup userConfigId newUserConfigs)
    unless
      (maybe ((iaAmount . ucCollateralizedInvestment) newState == (fromInteger 0)) ((ucCollateralizedInvestment newState ==) . ucCollateralizedInvestment) oldState &&
      (iaAmount . ucDebt $ newState) == (fromInteger 0) && maybe True ((== (fromInteger 0)) . iaAmount . ucDebt) oldState)
      (throwError "Change is not valid")

validateDeposit aave (ReservesDatum state reserves) ctx userConfigId =
  traceIfFalse "validateDeposit: Reserves Datum change is not valid" $ checkPositiveReservesTransformation state reserves ctx userConfigId

validateDeposit _ _ _ _ = trace "validateDeposit: Lending Pool Datum management is not allowed" False

{-# INLINABLE validateWithdraw #-}
validateWithdraw :: Aave -> AaveDatum -> ScriptContext -> UserConfigId -> Bool
validateWithdraw aave (UserConfigsDatum stateToken userConfigs) ctx userConfigId =
  -- TODO add implementation for this case
  traceIfFalse "validateWithdraw: User Configs Datum change is not valid" False
validateWithdraw aave (ReservesDatum stateToken reserves) ctx userConfigId =
  traceIfFalse "validateWithdraw: Reserves Datum change is not valid" $ checkNegativeReservesTransformation stateToken reserves ctx userConfigId

validateWithdraw aave ReserveFundsDatum ctx (reserveId, actor) =
  traceIfFalse "validateWithdraw: Reserve Funds Datum change is not valid" $ checkNegativeFundsTransformation ctx reserveId actor

validateWithdraw _ _ _ _ = trace "validateWithdraw: Lending Pool Datum management is not allowed" False

{-# INLINABLE validateBorrow #-}
validateBorrow :: Aave -> AaveDatum -> ScriptContext -> UserConfigId -> [(CurrencySymbol, PubKeyHash, Integer, AssetClass)] -> Slot -> Bool
validateBorrow aave (UserConfigsDatum state@AaveState{..} userConfigs) ctx userConfigId@(reserveId, actor) oracles slot =
  toBoolPrefixed "validateBorrow: " $ do
    assertValidCurrentSlot ctx slot
    newUserConfigs <- findUserConfigs ctx state
    assertInsertAt userConfigId userConfigs newUserConfigs

    let txInfo = scriptContextTxInfo ctx
    oracleValues <-
      case foldrM (\o@(_, _, _, oAsset) acc -> fmap ((: acc) . (oAsset, )) (Oracle.findOracleValueInTxInputs txInfo o)) [] oracles of
        Just vs -> pure . AssocMap.fromList $ vs
        _       -> throwError "Oracles have not been provided"
    reserve <- findReserves ctx state >>= maybe (throwError "Reserve not found") pure . AssocMap.lookup reserveId
    unless (doesCollateralCoverDebt actor oracleValues newUserConfigs) $ throwError "Not enough collateral"

    let oldState = AssocMap.lookup userConfigId userConfigs
    newState <- maybe (throwError "User config not found") pure $ AssocMap.lookup userConfigId newUserConfigs
    let accState = fmap (updateConfigAmounts reserve slot) oldState
        actorSpentValue = valueSpentFrom txInfo actor
        actorRemainderValue = valuePaidTo txInfo actor
        debtAmount = ((iaAmount . ucDebt) newState -) $ maybe (fromInteger 0) (iaAmount . ucDebt) accState
        disbursementAmount = fromInteger $ assetClassValueOf actorRemainderValue reserveId - assetClassValueOf actorSpentValue reserveId
    unless
      (debtAmount == disbursementAmount && debtAmount > fromInteger 0 && disbursementAmount > fromInteger 0 &&
          (iaAmount . ucCollateralizedInvestment $ newState) == (fromInteger 0) && maybe True ((== (fromInteger 0)) . iaAmount . ucCollateralizedInvestment) oldState)
      (throwError "Change is not valid")

validateBorrow aave (ReservesDatum stateToken reserves) ctx userConfigId oracles _ =
  traceIfFalse "validateBorrow: Reserves Datum change is not valid" $ checkNegativeReservesTransformation stateToken reserves ctx userConfigId && areOraclesTrusted oracles reserves

validateBorrow aave ReserveFundsDatum ctx (reserveId, actor) oracles _ =
  traceIfFalse "validateBorrow: Reserve Funds Datum change is not valid" $ checkNegativeFundsTransformation ctx reserveId actor

validateBorrow _ _ _ _ _ _ = trace "validateBorrow: Lending Pool Datum management is not allowed" False

{-# INLINABLE validateRepay #-}
validateRepay :: Aave -> AaveDatum -> ScriptContext -> UserConfigId -> Slot -> Bool
validateRepay aave (UserConfigsDatum state@AaveState{..} userConfigs) ctx userConfigId@(reserveId, actor) slot =
  toBoolPrefixed "validateRepay: " $ do
    assertValidCurrentSlot ctx slot
    newUserConfigs <- findUserConfigs ctx state
    assertInsertAt userConfigId userConfigs newUserConfigs
    reserve <- findReserves ctx state >>= maybe (throwError "Reserve not found") pure . AssocMap.lookup reserveId
    oldState <- maybe (throwError "User config not found") pure $ AssocMap.lookup userConfigId userConfigs
    newState <- maybe (throwError "User config not found") pure $ AssocMap.lookup userConfigId newUserConfigs
    let accState = updateConfigAmounts reserve slot oldState
        txInfo = scriptContextTxInfo ctx
        actorSpentValue = valueSpentFrom txInfo actor
        actorRemainderValue = valuePaidTo txInfo actor
        newDebt = iaAmount . ucDebt $ newState
        debtChange = (iaAmount . ucDebt) accState - newDebt
        reimbursementAmount = fromInteger $ assetClassValueOf actorSpentValue reserveId - assetClassValueOf actorRemainderValue reserveId
    unless
      (debtChange == reimbursementAmount && debtChange > fromInteger 0 && reimbursementAmount > fromInteger 0 && newDebt >= (fromInteger 0) &&
          ucCollateralizedInvestment newState == ucCollateralizedInvestment accState)
      (throwError "Change is not valid")

validateRepay aave (ReservesDatum stateToken reserves) ctx userConfigId _ =
  traceIfFalse "validateRepay: Reserves Datum change is not valid" $ checkPositiveReservesTransformation stateToken reserves ctx userConfigId

validateRepay _ _ _ _ _ = trace "validateRepay: Lending Pool Datum management is not allowed" False

{-# INLINABLE validateProvideCollateral #-}
validateProvideCollateral :: Aave -> AaveDatum -> ScriptContext -> UserConfigId -> Bool
validateProvideCollateral aave (UserConfigsDatum state@AaveState{..} userConfigs) ctx userConfigId@(reserveId, actor) =
  toBoolPrefixed "validateProvideCollateral: " $ do
    newUserConfigs <- findUserConfigs ctx state
    assertInsertAt userConfigId userConfigs newUserConfigs
    let txInfo = scriptContextTxInfo ctx
        actorSpentValue = valueSpentFrom txInfo actor
        actorRemainderValue = valuePaidTo txInfo actor
    (user, asset) <- maybe (throwError "Collateral not found") pure $
      findOnlyOneDatumByValue ctx (actorSpentValue - actorRemainderValue - txInfoFee txInfo) >>= pickUserCollateralFunds

    let oldState = AssocMap.lookup userConfigId userConfigs
    newState <- maybe (throwError "User config not found") pure $ AssocMap.lookup userConfigId newUserConfigs
    let investmentAmount = ((iaAmount . ucCollateralizedInvestment) newState -) $ maybe (fromInteger 0) (iaAmount . ucCollateralizedInvestment) oldState
        disbursementAmount = fromInteger $ assetClassValueOf actorSpentValue asset - assetClassValueOf actorRemainderValue asset
    unless
      (user == actor && investmentAmount == disbursementAmount && investmentAmount > fromInteger 0 && disbursementAmount > fromInteger 0 &&
          (iaAmount . ucDebt $ newState) == (fromInteger 0) && maybe True ((== (fromInteger 0)) . iaAmount . ucDebt) oldState)
      (throwError "Change is not valid")

validateProvideCollateral _ _ _ _ = trace "Lending Pool Datum management is not allowed" False

{-# INLINABLE validateRevokeCollateral #-}
validateRevokeCollateral :: Aave -> AaveDatum -> ScriptContext -> UserConfigId -> AssetClass -> [(CurrencySymbol, PubKeyHash, Integer, AssetClass)] -> Slot -> Bool
validateRevokeCollateral aave (UserConfigsDatum state@AaveState{..} userConfigs) ctx userConfigId@(reserveId, actor) aTokenAsset oracles slot =
  toBoolPrefixed "validateRevokeCollateral: " $ do
    assertValidCurrentSlot ctx slot
    newUserConfigs <- findUserConfigs ctx state
    let txInfo = scriptContextTxInfo ctx
    oracleValues <-
      case foldrM (\o@(_, _, _, oAsset) acc -> fmap ((: acc) . (oAsset, )) (Oracle.findOracleValueInTxInputs txInfo o)) [] oracles of
        Just vs -> pure . AssocMap.fromList $ vs
        _       -> throwError "Oracles have not been provided"
    unless (doesCollateralCoverDebt actor oracleValues newUserConfigs) $ throwError "Not enough collateral"

    oldState <- maybe (throwError "") pure $ AssocMap.lookup userConfigId userConfigs
    newState <- maybe (throwError "") pure $ AssocMap.lookup userConfigId newUserConfigs
    reserve <- findReserves ctx state >>= maybe (throwError "Reserve not found") pure . AssocMap.lookup reserveId
    let accState = updateConfigAmounts reserve slot oldState
        newInvestmentAmount = iaAmount . ucCollateralizedInvestment $ newState
        investmentShrinkedBy = (iaAmount . ucCollateralizedInvestment) accState - newInvestmentAmount
        actorSpentValue = valueSpentFrom txInfo actor
        actorRemainderValue = valuePaidTo txInfo actor
        disbursementAmount = fromInteger $ assetClassValueOf actorRemainderValue aTokenAsset - assetClassValueOf actorSpentValue aTokenAsset
    unless
      (investmentShrinkedBy == disbursementAmount && investmentShrinkedBy > fromInteger 0 &&
      disbursementAmount > fromInteger 0 && ucDebt newState == IncentivizedAmount slot (rCurrentStableBorrowRate reserve) (iaAmount . ucDebt $ accState))
      (throwError "Change is not valid")

validateRevokeCollateral aave  (UserCollateralFundsDatum owner aTokenAsset) ctx (reserveId, actor) revokedAsset oracles _ =
  traceIfFalse "validateRevokeCollateral: UserCollateralFundsDatum change is not valid" $
  owner == actor && revokedAsset == aTokenAsset && checkNegativeFundsTransformation ctx aTokenAsset actor

validateRevokeCollateral aave (ReservesDatum stateToken reserves) ctx userConfigId revokedAsset oracles _ =
  traceIfFalse "validateRevokeCollateral: Reserves Datum change is not valid" $ areOraclesTrusted oracles reserves

validateRevokeCollateral _ _ _ _ _ _ _ = trace "validateRevokeCollateral: Lending Pool Datum management is not allowed" False
