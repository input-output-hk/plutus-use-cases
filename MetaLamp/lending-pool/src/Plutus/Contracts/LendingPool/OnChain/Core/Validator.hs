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
import           Plutus.Contracts.LendingPool.OnChain.Core.Logic  (areOraclesTrusted,
                                                                   checkNegativeFundsTransformation,
                                                                   checkNegativeReservesTransformation,
                                                                   checkPositiveReservesTransformation,
                                                                   doesCollateralCoverDebt,
                                                                   pickReserves,
                                                                   pickUserCollateralFunds,
                                                                   pickUserConfigs)
import           Plutus.Contracts.LendingPool.OnChain.Core.Script (AaveDatum (..),
                                                                   AaveRedeemer (..),
                                                                   AaveScript,
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

newtype Aave = Aave
    { aaveProtocolInst :: AssetClass
    } deriving stock    (Prelude.Eq, Show, Generic)
      deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''Aave
PlutusTx.makeLift ''Aave

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
makeAaveValidator :: Aave
                   -> AaveDatum
                   -> AaveRedeemer
                   -> ScriptContext
                   -> Bool
makeAaveValidator aave datum StartRedeemer ctx    = trace "StartRedeemer" $ validateStart aave datum ctx
-- TODO ? further validators should check that ReservesDatum & UserConfigsDatum transormation happens one time
-- & ReserveFundsDatum transormation happens at least one time
-- TODO ? check that reedeemers contain the same data during transformation
-- TODO validate that userConfigId and reserveId are the only datum changed in trasformation and other users datum is not modified
makeAaveValidator aave datum (DepositRedeemer userConfigId) ctx  = trace "DepositRedeemer" $ validateDeposit aave datum ctx userConfigId
makeAaveValidator aave datum (WithdrawRedeemer userConfigId) ctx = trace "WithdrawRedeemer" $ validateWithdraw aave datum ctx userConfigId
makeAaveValidator aave datum (BorrowRedeemer userConfigId oracles) ctx   = trace "BorrowRedeemer" $ validateBorrow aave datum ctx userConfigId oracles
makeAaveValidator aave datum (RepayRedeemer userConfigId) ctx    = trace "RepayRedeemer" $ validateRepay aave datum ctx userConfigId
makeAaveValidator aave datum (ProvideCollateralRedeemer userConfigId) ctx    = trace "ProvideCollateralRedeemer" $ validateProvideCollateral aave datum ctx userConfigId
makeAaveValidator aave datum (RevokeCollateralRedeemer userConfigId aTokenAsset oracles) ctx    = trace "RevokeCollateralRedeemer" $ validateRevokeCollateral aave datum ctx userConfigId aTokenAsset oracles

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
validateDeposit aave (UserConfigsDatum stateToken userConfigs) ctx userConfigId =
  traceIfFalse "validateDeposit: User Configs Datum change is not valid" isValidUserConfigsTransformation
  where
    txInfo = scriptContextTxInfo ctx
    (scriptsHash, scriptsDatumHash) = ownHashes ctx
    userConfigsOutputDatumHash =
      findOnlyOneDatumHashByValue (assetClassValue stateToken 1) $ scriptOutputsAt scriptsHash txInfo
    userConfigsOutputDatum ::
         Maybe (AssetClass, AssocMap.Map UserConfigId UserConfig)
    userConfigsOutputDatum =
      userConfigsOutputDatumHash >>= parseDatum txInfo >>= pickUserConfigs

    isValidUserConfigsTransformation :: Bool
    isValidUserConfigsTransformation =
      maybe False checkUserConfigs userConfigsOutputDatum
    checkUserConfigs :: (AssetClass, AssocMap.Map UserConfigId UserConfig) -> Bool
    checkUserConfigs (newStateToken, newUserConfigs) =
      newStateToken == stateToken &&
      maybe
        False
        (checkRedeemerConfig (AssocMap.lookup userConfigId userConfigs))
        (AssocMap.lookup userConfigId newUserConfigs)

    checkRedeemerConfig :: Maybe UserConfig -> UserConfig -> Bool
    checkRedeemerConfig oldState newState =
      maybe (ucCollateralizedInvestment newState == 0) ((ucCollateralizedInvestment newState ==) . ucCollateralizedInvestment) oldState &&
      ucDebt newState == 0 && maybe True ((== 0) . ucDebt) oldState

validateDeposit aave (ReservesDatum stateToken reserves) ctx userConfigId =
  traceIfFalse "validateDeposit: Reserves Datum change is not valid" $ checkPositiveReservesTransformation stateToken reserves ctx userConfigId

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
validateBorrow :: Aave -> AaveDatum -> ScriptContext -> UserConfigId -> [(CurrencySymbol, PubKeyHash, Integer, AssetClass)] -> Bool
validateBorrow aave (UserConfigsDatum stateToken userConfigs) ctx userConfigId@(reserveId, actor) oracles =
  traceIfFalse "validateBorrow: User Configs Datum change is not valid" isValidUserConfigsTransformation
  where
    txInfo = scriptContextTxInfo ctx
    (scriptsHash, scriptsDatumHash) = ownHashes ctx
    scriptOutputs = scriptOutputsAt scriptsHash txInfo
    userConfigsOutputDatumHash =
      findOnlyOneDatumHashByValue (assetClassValue stateToken 1) scriptOutputs
    userConfigsOutputDatum ::
         Maybe (AssetClass, AssocMap.Map UserConfigId UserConfig)
    userConfigsOutputDatum =
      userConfigsOutputDatumHash >>= parseDatum txInfo >>= pickUserConfigs

    actorSpentValue = valueSpentFrom txInfo actor
    actorRemainderValue = valuePaidTo txInfo actor

    oracleValues =
      case foldrM (\o@(_, _, _, oAsset) acc -> fmap ((: acc) . (oAsset, )) (Oracle.findOracleValueInTxInputs txInfo o)) [] oracles of
        Just vs -> AssocMap.fromList vs
        _       -> traceError "validateBorrow: Oracles have not been provided"

    isValidUserConfigsTransformation :: Bool
    isValidUserConfigsTransformation =
      maybe False checkUserConfigs userConfigsOutputDatum
    checkUserConfigs ::
         (AssetClass, AssocMap.Map UserConfigId UserConfig) -> Bool
    checkUserConfigs (newStateToken, newUserConfigs) =
      newStateToken == stateToken && doesCollateralCoverDebt actor oracleValues newUserConfigs &&
      maybe False (checkRedeemerConfig $ AssocMap.lookup userConfigId userConfigs) (AssocMap.lookup userConfigId newUserConfigs)
    checkRedeemerConfig :: Maybe UserConfig -> UserConfig -> Bool
    checkRedeemerConfig oldState newState =
      let debtAmount = (ucDebt newState -) $ maybe 0 ucDebt oldState
          disbursementAmount = assetClassValueOf actorRemainderValue reserveId - assetClassValueOf actorSpentValue reserveId
       in debtAmount == disbursementAmount && debtAmount > 0 && disbursementAmount > 0 &&
          ucCollateralizedInvestment newState == 0 && maybe True ((== 0) . ucCollateralizedInvestment) oldState

validateBorrow aave (ReservesDatum stateToken reserves) ctx userConfigId oracles =
  traceIfFalse "validateBorrow: Reserves Datum change is not valid" $ checkNegativeReservesTransformation stateToken reserves ctx userConfigId && areOraclesTrusted oracles reserves

validateBorrow aave ReserveFundsDatum ctx (reserveId, actor) oracles =
  traceIfFalse "validateBorrow: Reserve Funds Datum change is not valid" $ checkNegativeFundsTransformation ctx reserveId actor

validateBorrow _ _ _ _ _ = trace "validateBorrow: Lending Pool Datum management is not allowed" False

{-# INLINABLE validateRepay #-}
validateRepay :: Aave -> AaveDatum -> ScriptContext -> UserConfigId -> Bool
validateRepay aave (UserConfigsDatum stateToken userConfigs) ctx userConfigId@(reserveId, actor) =
  traceIfFalse "validateRepay: User Configs Datum change is not valid" isValidUserConfigsTransformation
  where
    txInfo = scriptContextTxInfo ctx
    (scriptsHash, scriptsDatumHash) = ownHashes ctx
    scriptOutputs = scriptOutputsAt scriptsHash txInfo
    userConfigsOutputDatumHash =
      findOnlyOneDatumHashByValue (assetClassValue stateToken 1) scriptOutputs
    userConfigsOutputDatum ::
         Maybe (AssetClass, AssocMap.Map UserConfigId UserConfig)
    userConfigsOutputDatum =
      userConfigsOutputDatumHash >>= parseDatum txInfo >>= pickUserConfigs

    actorSpentValue = valueSpentFrom txInfo actor
    actorRemainderValue = valuePaidTo txInfo actor

    isValidUserConfigsTransformation :: Bool
    isValidUserConfigsTransformation =
      maybe False checkUserConfigs userConfigsOutputDatum
    checkUserConfigs :: (AssetClass, AssocMap.Map UserConfigId UserConfig) -> Bool
    checkUserConfigs (newStateToken, newUserConfigs) =
      newStateToken == stateToken &&
      (Just True ==
       (checkRedeemerConfig <$> AssocMap.lookup userConfigId userConfigs <*> AssocMap.lookup userConfigId newUserConfigs))
    checkRedeemerConfig :: UserConfig -> UserConfig -> Bool
    checkRedeemerConfig oldState newState =
      let newDebt = ucDebt newState
          debtChange = ucDebt oldState - newDebt
          reimbursementAmount = assetClassValueOf actorSpentValue reserveId - assetClassValueOf actorRemainderValue reserveId
       in debtChange == reimbursementAmount && debtChange > 0 && reimbursementAmount > 0 && newDebt >= 0 &&
          ucCollateralizedInvestment newState == ucCollateralizedInvestment oldState

validateRepay aave (ReservesDatum stateToken reserves) ctx userConfigId =
  traceIfFalse "validateRepay: Reserves Datum change is not valid" $ checkPositiveReservesTransformation stateToken reserves ctx userConfigId

validateRepay _ _ _ _ = trace "validateRepay: Lending Pool Datum management is not allowed" False

{-# INLINABLE validateProvideCollateral #-}
validateProvideCollateral :: Aave -> AaveDatum -> ScriptContext -> UserConfigId -> Bool
validateProvideCollateral aave  (UserConfigsDatum stateToken userConfigs) ctx userConfigId@(reserveId, actor) =
  traceIfFalse "validateProvideCollateral: User Configs Datum change is not valid" isValidUserConfigsTransformation
  where
    txInfo = scriptContextTxInfo ctx
    (scriptsHash, scriptsDatumHash) = ownHashes ctx
    scriptOutputs = scriptOutputsAt scriptsHash txInfo

    userConfigsOutputDatumHash =
      findOnlyOneDatumHashByValue (assetClassValue stateToken 1) scriptOutputs
    userConfigsOutputDatum ::
         Maybe (AssetClass, AssocMap.Map UserConfigId UserConfig)
    userConfigsOutputDatum =
      userConfigsOutputDatumHash >>= parseDatum txInfo >>= pickUserConfigs

    actorSpentValue = valueSpentFrom txInfo actor
    actorRemainderValue = valuePaidTo txInfo actor

    collateralOutputDatumHash =
      findOnlyOneDatumHashByValue (actorSpentValue - actorRemainderValue - txInfoFee txInfo) scriptOutputs
    collateralOutputDatum ::
         Maybe (PubKeyHash, AssetClass)
    collateralOutputDatum =
      collateralOutputDatumHash >>= parseDatum txInfo >>= pickUserCollateralFunds

    isValidUserConfigsTransformation :: Bool
    isValidUserConfigsTransformation =
      fromMaybe False $ checkUserConfigs <$> userConfigsOutputDatum <*> collateralOutputDatum
    checkUserConfigs ::
         (AssetClass, AssocMap.Map UserConfigId UserConfig) -> (PubKeyHash, AssetClass) -> Bool
    checkUserConfigs (newStateToken, newUserConfigs) (user, aTokenAsset) =
      newStateToken == stateToken && user == actor &&
      maybe False (checkRedeemerConfig aTokenAsset $ AssocMap.lookup userConfigId userConfigs) (AssocMap.lookup userConfigId newUserConfigs)
    checkRedeemerConfig :: AssetClass -> Maybe UserConfig -> UserConfig -> Bool
    checkRedeemerConfig asset oldState newState =
      let investmentAmount = (ucCollateralizedInvestment newState -) $ maybe 0 ucCollateralizedInvestment oldState
          disbursementAmount = assetClassValueOf actorSpentValue asset - assetClassValueOf actorRemainderValue asset
       in investmentAmount == disbursementAmount && investmentAmount > 0 && disbursementAmount > 0 &&
          ucDebt newState == 0 && maybe True ((== 0) . ucDebt) oldState

validateProvideCollateral _ _ _ _ = trace "validateProvideCollateral: Lending Pool Datum management is not allowed" False

{-# INLINABLE validateRevokeCollateral #-}
validateRevokeCollateral :: Aave -> AaveDatum -> ScriptContext -> UserConfigId -> AssetClass -> [(CurrencySymbol, PubKeyHash, Integer, AssetClass)] -> Bool
validateRevokeCollateral aave  (UserConfigsDatum stateToken userConfigs) ctx userConfigId@(reserveId, actor) aTokenAsset oracles =
  traceIfFalse "validateRevokeCollateral: User Configs Datum change is not valid" isValidUserConfigsTransformation
  where
    txInfo = scriptContextTxInfo ctx
    (scriptsHash, scriptsDatumHash) = ownHashes ctx
    scriptOutputs = scriptOutputsAt scriptsHash txInfo

    userConfigsOutputDatumHash =
      findOnlyOneDatumHashByValue (assetClassValue stateToken 1) scriptOutputs
    userConfigsOutputDatum ::
         Maybe (AssetClass, AssocMap.Map UserConfigId UserConfig)
    userConfigsOutputDatum =
      userConfigsOutputDatumHash >>= parseDatum txInfo >>= pickUserConfigs

    actorSpentValue = valueSpentFrom txInfo actor
    actorRemainderValue = valuePaidTo txInfo actor

    oracleValues =
      case foldrM (\o@(_, _, _, oAsset) acc -> fmap ((: acc) . (oAsset, )) (Oracle.findOracleValueInTxInputs txInfo o)) [] oracles of
        Just vs -> AssocMap.fromList vs
        _ -> traceError "validateRevokeCollateral: Oracles have not been provided"

    isValidUserConfigsTransformation :: Bool
    isValidUserConfigsTransformation =
      maybe False checkUserConfigs userConfigsOutputDatum
    checkUserConfigs ::
         (AssetClass, AssocMap.Map UserConfigId UserConfig) -> Bool
    checkUserConfigs (newStateToken, newUserConfigs) =
      newStateToken == stateToken && doesCollateralCoverDebt actor oracleValues newUserConfigs &&
      fromMaybe False (checkRedeemerConfig <$> (AssocMap.lookup userConfigId userConfigs) <*> (AssocMap.lookup userConfigId newUserConfigs))
    checkRedeemerConfig :: UserConfig -> UserConfig -> Bool
    checkRedeemerConfig oldState newState =
      let newInvestmentAmount = ucCollateralizedInvestment newState
          investmentShrinkedBy = ucCollateralizedInvestment oldState - newInvestmentAmount
          disbursementAmount = assetClassValueOf actorRemainderValue aTokenAsset - assetClassValueOf actorSpentValue aTokenAsset
       in investmentShrinkedBy == disbursementAmount && investmentShrinkedBy > 0 && disbursementAmount > 0 && newInvestmentAmount >= 0 &&
          (ucDebt newState == ucDebt oldState)

validateRevokeCollateral aave  (UserCollateralFundsDatum owner aTokenAsset) ctx (reserveId, actor) revokedAsset oracles =
  traceIfFalse "validateRevokeCollateral: UserCollateralFundsDatum change is not valid" $
  owner == actor && revokedAsset == aTokenAsset && checkNegativeFundsTransformation ctx aTokenAsset actor

validateRevokeCollateral aave (ReservesDatum stateToken reserves) ctx userConfigId revokedAsset oracles =
  traceIfFalse "validateRevokeCollateral: Reserves Datum change is not valid" $ areOraclesTrusted oracles reserves

validateRevokeCollateral _ _ _ _ _ _ = trace "validateRevokeCollateral: Lending Pool Datum management is not allowed" False
