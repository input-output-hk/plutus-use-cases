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
import qualified Ledger.Typed.Scripts                             as Scripts
import           Playground.Contract
import           Plutus.Abstract.IncentivizedAmount               (IncentivizedAmount (..),
                                                                   accrue)
import           Plutus.Contract                                  hiding (when)
import qualified Plutus.Contracts.LendingPool.InterestRate        as InterestRate
import           Plutus.Contracts.LendingPool.OnChain.Core.Logic  (areOraclesTrusted,
                                                                   assertInsertAt,
                                                                   assertValidCurrentSlot,
                                                                   checkNegativeFundsTransformation,
                                                                   checkNegativeReservesTransformation,
                                                                   checkPositiveReservesTransformation,
                                                                   doesCollateralCoverDebt,
                                                                   findAaveState,
                                                                   findReserves,
                                                                   findUserConfigs,
                                                                   pickReserves,
                                                                   pickUserCollateralFunds,
                                                                   pickUserConfigs,
                                                                   toBool,
                                                                   toBoolPrefixed)
import           Plutus.Contracts.LendingPool.OnChain.Core.Script (Aave (..),
                                                                   AaveDatum (..),
                                                                   AaveNewState (..),
                                                                   AaveRedeemer (..),
                                                                   AaveScript,
                                                                   AaveState (..),
                                                                   Reserve (..),
                                                                   UserConfig (..),
                                                                   UserConfigId,
                                                                   _ucCollateralizedInvestment)
import           Plutus.Contracts.LendingPool.Shared              (UpdateConfigParams (..),
                                                                   updateConfigAmounts)
import qualified Plutus.Contracts.LendingPool.Shared              as Shared
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
makeAaveValidator _ _ _ _ = True
{-
makeAaveValidator aave datum StartRedeemer ctx = trace "StartRedeemer" $ validateStart aave datum ctx
makeAaveValidator aave datum (DepositRedeemer userConfigId) ctx  = trace "DepositRedeemer" $ validateDeposit aave datum ctx userConfigId
makeAaveValidator aave datum (WithdrawRedeemer userConfigId) ctx = trace "WithdrawRedeemer" $ validateWithdraw aave datum ctx userConfigId
makeAaveValidator aave datum (BorrowRedeemer amount userConfigId oracles slot) ctx = trace "BorrowRedeemer" $ validateBorrow aave datum ctx amount userConfigId oracles slot
makeAaveValidator aave datum (RepayRedeemer amount userConfigId slot) ctx = trace "RepayRedeemer" $ validateRepay aave datum ctx amount userConfigId slot
makeAaveValidator aave datum (ProvideCollateralRedeemer userConfigId slot) ctx = trace "ProvideCollateralRedeemer" $ validateProvideCollateral aave datum ctx userConfigId slot
makeAaveValidator aave datum (RevokeCollateralRedeemer userConfigId aTokenAsset oracles slot) ctx = trace "RevokeCollateralRedeemer" $ validateRevokeCollateral aave datum ctx userConfigId aTokenAsset oracles slot
-}

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
validateDeposit aave (StateDatum stateToken oldState) ctx userConfigId =
  toBoolPrefixed "validateDeposit: " $ do
    newState <- findAaveState ctx stateToken
    assertInsertAt userConfigId (ansUserConfigs oldState) (ansUserConfigs newState)

    let oldUserConfig = AssocMap.lookup userConfigId (ansUserConfigs oldState)
    newUserConfig <- maybe (throwError "User config not found") pure (AssocMap.lookup userConfigId (ansUserConfigs newState))
    unless
      (maybe ((iaAmount . ucCollateralizedInvestment) newUserConfig == (fromInteger 0)) ((ucCollateralizedInvestment newUserConfig ==) . ucCollateralizedInvestment) oldUserConfig
        && (iaAmount . ucDebt $ newUserConfig) == (fromInteger 0) && maybe True ((== (fromInteger 0)) . iaAmount . ucDebt) oldUserConfig
        && checkPositiveReservesTransformation (ansReserves oldState) (ansReserves newState) ctx userConfigId)
      (throwError "Change is not valid")

validateDeposit _ _ _ _ = trace "validateDeposit: Lending Pool Datum management is not allowed" False

{-# INLINABLE validateWithdraw #-}
validateWithdraw :: Aave -> AaveDatum -> ScriptContext -> UserConfigId -> Bool
validateWithdraw aave (StateDatum stateToken oldState) ctx userConfigId =
  toBoolPrefixed "validateWithdraw: " $ do
    newState <- findAaveState ctx stateToken

    unless
      (checkNegativeReservesTransformation (ansReserves oldState) (ansReserves newState) ctx userConfigId)
      (throwError "")

validateWithdraw aave ReserveFundsDatum ctx (reserveId, actor) =
  traceIfFalse "validateWithdraw: Reserve Funds Datum change is not valid" $ checkNegativeFundsTransformation ctx reserveId actor

validateWithdraw _ _ _ _ = trace "validateWithdraw: Lending Pool Datum management is not allowed" False

{-# INLINABLE validateBorrow #-}
validateBorrow :: Aave -> AaveDatum -> ScriptContext -> Integer -> UserConfigId -> [(CurrencySymbol, PubKeyHash, Integer, AssetClass)] -> Slot -> Bool
validateBorrow aave (StateDatum stateToken oldState) ctx borrowAmount userConfigId@(reserveId, actor) oracles slot =
  toBoolPrefixed "validateBorrow: " $ do
    assertValidCurrentSlot ctx slot
    newState <- findAaveState ctx stateToken
    assertInsertAt userConfigId (ansUserConfigs oldState) (ansUserConfigs newState)
    assertInsertAt reserveId (ansReserves oldState) (ansReserves newState)

    let txInfo = scriptContextTxInfo ctx
    oracleValues <-
      case foldrM (\o@(_, _, _, oAsset) acc -> fmap ((: acc) . (oAsset, )) (Oracle.findOracleValueInTxInputs txInfo o)) [] oracles of
        Just vs -> pure . AssocMap.fromList $ vs
        _       -> throwError "Oracles have not been provided"

    oldReserve <- maybe (throwError "Reserve not found") pure . AssocMap.lookup reserveId . ansReserves $ oldState
    newReserve <- maybe (throwError "Reserve not found") pure . AssocMap.lookup reserveId . ansReserves $ newState

    fundsValue <- maybe (throwError "Datum hash not found") pure $ findValueByDatum ctx ReserveFundsDatum
    let availableLiquidity = flip assetClassValueOf reserveId fundsValue + borrowAmount
    let reserveConfigs = fmap snd . filter (\((asset, _), _) -> asset == reserveId) . AssocMap.toList . ansUserConfigs $ oldState
    let updatedReserve = Shared.updateReserveOnBorrow reserveConfigs availableLiquidity borrowAmount slot oldReserve

    unless (updatedReserve == newReserve) (throwError "Invalid reserve calculation")
    unless (doesCollateralCoverDebt actor oracleValues (ansReserves newState) slot (ansUserConfigs newState)) $ throwError "Not enough collateral"
    unless (areOraclesTrusted oracles (ansReserves oldState)) (throwError "Invalid oracles")

    let oldUserConfig = AssocMap.lookup userConfigId (ansUserConfigs oldState)
    newUserConfig <- maybe (throwError "User config not found") pure $ AssocMap.lookup userConfigId (ansUserConfigs newState)
    let accUserConfig = fmap (updateConfigAmounts UpdateConfigParams { ucpUpdatedReserve = updatedReserve, ucpPreviousReserveUpdated = rLastUpdated oldReserve, ucpCurrentSlot = slot }) oldUserConfig
        actorSpentValue = valueSpentFrom txInfo actor
        actorRemainderValue = valuePaidTo txInfo actor
        debtAmount = ((iaAmount . ucDebt) newUserConfig -) $ maybe (fromInteger 0) (iaAmount . ucDebt) accUserConfig
        disbursementAmount = fromInteger $ assetClassValueOf actorRemainderValue reserveId - assetClassValueOf actorSpentValue reserveId
    unless
      (debtAmount == disbursementAmount && debtAmount > fromInteger 0 && disbursementAmount > fromInteger 0 &&
          (iaAmount . ucCollateralizedInvestment $ newUserConfig) == (fromInteger 0) && maybe True ((== (fromInteger 0)) . iaAmount . ucCollateralizedInvestment) oldUserConfig)
      (throwError "Change is not valid")

validateBorrow aave ReserveFundsDatum ctx amount (reserveId, actor) oracles _ =
  traceIfFalse "validateBorrow: Reserve Funds Datum change is not valid" $ checkNegativeFundsTransformation ctx reserveId actor

validateBorrow _ _ _ _ _ _ _ = trace "validateBorrow: Lending Pool Datum management is not allowed" False

{-# INLINABLE validateRepay #-}
validateRepay :: Aave -> AaveDatum -> ScriptContext -> Integer -> UserConfigId -> Slot -> Bool
validateRepay aave (StateDatum stateToken oldState) ctx repayAmount userConfigId@(reserveId, actor) slot =
  toBoolPrefixed "validateRepay: " $ do
    assertValidCurrentSlot ctx slot
    newState <- findAaveState ctx stateToken
    assertInsertAt userConfigId (ansUserConfigs oldState) (ansUserConfigs newState)
    assertInsertAt reserveId (ansReserves oldState) (ansReserves newState)

    oldReserve <- maybe (throwError "Reserve not found") pure . AssocMap.lookup reserveId . ansReserves $ oldState
    newReserve <- maybe (throwError "Reserve not found") pure . AssocMap.lookup reserveId . ansReserves $ newState

    fundsValue <- maybe (throwError "Datum hash not found") pure $ findValueByDatum ctx ReserveFundsDatum
    let availableLiquidity = flip assetClassValueOf reserveId fundsValue - repayAmount
    let reserveConfigs = fmap snd . filter (\((asset, _), _) -> asset == reserveId) . AssocMap.toList . ansUserConfigs $ oldState
    let updatedReserve = Shared.updateReserveOnRepay reserveConfigs availableLiquidity repayAmount slot oldReserve

    unless (updatedReserve == newReserve) (throwError "Invalid reserve calculation")

    oldUserConfig <- maybe (throwError "User config not found") pure $ AssocMap.lookup userConfigId (ansUserConfigs oldState)
    newUserConfig <- maybe (throwError "User config not found") pure $ AssocMap.lookup userConfigId (ansUserConfigs newState)

    let accUserConfig = updateConfigAmounts UpdateConfigParams { ucpUpdatedReserve = updatedReserve, ucpPreviousReserveUpdated = rLastUpdated oldReserve, ucpCurrentSlot = slot } oldUserConfig
        txInfo = scriptContextTxInfo ctx
        actorSpentValue = valueSpentFrom txInfo actor
        actorRemainderValue = valuePaidTo txInfo actor
        newDebt = iaAmount . ucDebt $ newUserConfig
        debtChange = (iaAmount . ucDebt) accUserConfig - newDebt
        reimbursementAmount = fromInteger $ assetClassValueOf actorSpentValue reserveId - assetClassValueOf actorRemainderValue reserveId
    unless
      (debtChange == reimbursementAmount && debtChange > fromInteger 0 && reimbursementAmount > fromInteger 0 && newDebt >= (fromInteger 0) &&
          ucCollateralizedInvestment newUserConfig == ucCollateralizedInvestment accUserConfig)
      (throwError "Change is not valid")

validateRepay _ _ _ _ _ _ = trace "validateRepay: Lending Pool Datum management is not allowed" False

{-# INLINABLE validateProvideCollateral #-}
validateProvideCollateral :: Aave -> AaveDatum -> ScriptContext -> UserConfigId -> Slot -> Bool
validateProvideCollateral aave (StateDatum stateToken oldState) ctx userConfigId@(reserveId, actor) slot =
  toBoolPrefixed "validateProvideCollateral: " $ do
    assertValidCurrentSlot ctx slot
    newState <- findAaveState ctx stateToken
    assertInsertAt userConfigId (ansUserConfigs oldState) (ansUserConfigs newState)
    assertInsertAt reserveId (ansReserves oldState) (ansReserves newState)

    let txInfo = scriptContextTxInfo ctx
        actorSpentValue = valueSpentFrom txInfo actor
        actorRemainderValue = valuePaidTo txInfo actor
    (user, asset) <- maybe (throwError "Collateral not found") pure $
      findOnlyOneDatumByValue ctx (actorSpentValue - actorRemainderValue - txInfoFee txInfo) >>= pickUserCollateralFunds

    oldReserve <- maybe (throwError "Reserve not found") pure . AssocMap.lookup reserveId . ansReserves $ oldState
    newReserve <- maybe (throwError "Reserve not found") pure . AssocMap.lookup reserveId . ansReserves $ newState
    fundsValue <- maybe (pure mempty) pure $ findValueByDatum ctx ReserveFundsDatum
    let availableLiquidity = flip assetClassValueOf reserveId fundsValue
    let reserveConfigs = fmap snd . filter (\((asset, _), _) -> asset == reserveId) . AssocMap.toList . ansUserConfigs $ oldState
    let updatedReserve = Shared.updateReserveOnLiquidityChange reserveConfigs availableLiquidity slot oldReserve

    let oldUserConfig = AssocMap.lookup userConfigId (ansUserConfigs oldState)
    newUserConfig <- maybe (throwError "User config not found") pure $ AssocMap.lookup userConfigId (ansUserConfigs newState)
    let accUserConfig = updateConfigAmounts UpdateConfigParams { ucpUpdatedReserve = updatedReserve, ucpPreviousReserveUpdated = rLastUpdated oldReserve, ucpCurrentSlot = slot } <$> oldUserConfig
        investmentAmount = ((iaAmount . ucCollateralizedInvestment) newUserConfig -) $ maybe (fromInteger 0) (iaAmount . ucCollateralizedInvestment) oldUserConfig
        disbursementAmount = fromInteger $ assetClassValueOf actorSpentValue asset - assetClassValueOf actorRemainderValue asset
    unless
      (user == actor && investmentAmount == disbursementAmount && investmentAmount > fromInteger 0 && disbursementAmount > fromInteger 0 &&
          (iaAmount . ucDebt $ newUserConfig) == (fromInteger 0) && maybe True ((== (fromInteger 0)) . iaAmount . ucDebt) oldUserConfig)
      (throwError "Change is not valid")

validateProvideCollateral _ _ _ _ _ = trace "Lending Pool Datum management is not allowed" False

{-# INLINABLE validateRevokeCollateral #-}
validateRevokeCollateral :: Aave -> AaveDatum -> ScriptContext -> UserConfigId -> AssetClass -> [(CurrencySymbol, PubKeyHash, Integer, AssetClass)] -> Slot -> Bool
validateRevokeCollateral aave (StateDatum stateToken oldState) ctx userConfigId@(reserveId, actor) aTokenAsset oracles slot =
  toBoolPrefixed "validateRevokeCollateral: " $ do
    assertValidCurrentSlot ctx slot
    newState <- findAaveState ctx stateToken
    assertInsertAt userConfigId (ansUserConfigs oldState) (ansUserConfigs newState)
    assertInsertAt reserveId (ansReserves oldState) (ansReserves newState)

    let txInfo = scriptContextTxInfo ctx
    oracleValues <-
      case foldrM (\o@(_, _, _, oAsset) acc -> fmap ((: acc) . (oAsset, )) (Oracle.findOracleValueInTxInputs txInfo o)) [] oracles of
        Just vs -> pure . AssocMap.fromList $ vs
        _       -> throwError "Oracles have not been provided"
    unless (doesCollateralCoverDebt actor oracleValues (ansReserves newState) slot (ansUserConfigs newState)) $ throwError "Not enough collateral"
    unless (areOraclesTrusted oracles (ansReserves oldState)) (throwError "Invalid oracles")

    oldReserve <- maybe (throwError "Reserve not found") pure . AssocMap.lookup reserveId . ansReserves $ oldState
    newReserve <- maybe (throwError "Reserve not found") pure . AssocMap.lookup reserveId . ansReserves $ newState
    fundsValue <- maybe (throwError "Datum hash not found") pure $ findValueByDatum ctx ReserveFundsDatum
    let availableLiquidity = flip assetClassValueOf reserveId fundsValue
    let reserveConfigs = fmap snd . filter (\((asset, _), _) -> asset == reserveId) . AssocMap.toList . ansUserConfigs $ oldState
    let updatedReserve = Shared.updateReserveOnLiquidityChange reserveConfigs availableLiquidity slot oldReserve

    oldUserConfig <- maybe (throwError "User config not found") pure $ AssocMap.lookup userConfigId (ansUserConfigs oldState)
    newUserConfig <- maybe (throwError "User config not found") pure $ AssocMap.lookup userConfigId (ansUserConfigs newState)
    let accUserConfig = updateConfigAmounts UpdateConfigParams { ucpUpdatedReserve = updatedReserve, ucpPreviousReserveUpdated = rLastUpdated oldReserve, ucpCurrentSlot = slot } oldUserConfig
        newInvestmentAmount = iaAmount . ucCollateralizedInvestment $ newUserConfig
        investmentShrinkedBy = (iaAmount . ucCollateralizedInvestment) accUserConfig - newInvestmentAmount
        actorSpentValue = valueSpentFrom txInfo actor
        actorRemainderValue = valuePaidTo txInfo actor
        disbursementAmount = fromInteger $ assetClassValueOf actorRemainderValue aTokenAsset - assetClassValueOf actorSpentValue aTokenAsset
    unless
      (investmentShrinkedBy == disbursementAmount && investmentShrinkedBy > fromInteger 0 &&
      disbursementAmount > fromInteger 0 && ucDebt newUserConfig == IncentivizedAmount slot (rCurrentStableBorrowRate updatedReserve) (iaAmount . ucDebt $ accUserConfig))
      (throwError "Change is not valid")

validateRevokeCollateral aave  (UserCollateralFundsDatum owner aTokenAsset) ctx (reserveId, actor) revokedAsset oracles _ =
  traceIfFalse "validateRevokeCollateral: UserCollateralFundsDatum change is not valid" $
  owner == actor && revokedAsset == aTokenAsset && checkNegativeFundsTransformation ctx aTokenAsset actor

validateRevokeCollateral _ _ _ _ _ _ _ = trace "validateRevokeCollateral: Lending Pool Datum management is not allowed" False
