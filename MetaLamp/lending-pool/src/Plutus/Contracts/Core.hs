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
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Plutus.Contracts.Core where

import           Control.Lens                     ((^?))
import qualified Control.Lens                     as Lens
import           Control.Monad                    hiding (fmap)
import qualified Data.ByteString                  as BS
import qualified Data.Map                         as Map
import           Data.Text                        (Text, pack)
import           Data.Void                        (Void)
import           Ext.Plutus.Ledger.Contexts       (findOnlyOneDatumHashByValue,
                                                   findValueByDatumHash,
                                                   parseDatum, scriptInputsAt,
                                                   valueSpentFrom)
import           Ledger                           hiding (singleton)
import           Ledger.Constraints               as Constraints
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import qualified Ledger.Scripts                   as UntypedScripts
import qualified Ledger.Typed.Scripts             as Scripts
import           Playground.Contract
import           Plutus.Contract                  hiding (when)
import           Plutus.V1.Ledger.Value
import qualified PlutusTx
import qualified PlutusTx.AssocMap                as AssocMap
import           PlutusTx.Prelude                 hiding (Semigroup (..),
                                                   unless)
import           Prelude                          (Semigroup (..))
import qualified Prelude

newtype Aave = Aave
    { aaveProtocolInst :: AssetClass
    } deriving stock    (Prelude.Eq, Show, Generic)
      deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''Aave

deriving anyclass instance ToSchema Rational

data Reserve = Reserve
    { rCurrency                :: AssetClass, -- reserve id
      rAToken                  :: AssetClass,
      rAmount                  :: Integer,
      rLiquidityIndex          :: Integer,
      rCurrentStableBorrowRate :: Rational
    }
    deriving stock (Prelude.Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''Reserve
PlutusTx.makeLift ''Reserve

-- TODO (?) only aTokens pledged as collateral should accumulate interest
-- data UserConfig = UserConfig
--   { ucDebt                     :: [IncentivizedAmount]
--   , ucCollateralizedInvestment :: [IncentivizedAmount]
--   }
-- data IncentivizedAmount = IncentivizedAmount
--   { iaAmount :: Integer
--   , iaRate   :: Rational
--   , iaSlot   :: Slot
--   }

data UserConfig = UserConfig
    {
      ucDebt              :: Integer,
      ucCollateralizedInvestment :: Integer

    }
    deriving stock (Prelude.Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''UserConfig
PlutusTx.makeLift ''UserConfig

data AaveRedeemer =
    StartRedeemer
  | DepositRedeemer (AssetClass, PubKeyHash)
  | WithdrawRedeemer (AssetClass, PubKeyHash)
  | BorrowRedeemer (AssetClass, PubKeyHash) -- TODO we need to check amountOfCollateralNeededLovelace <= userCollateralBalanceLovelace
  | RepayRedeemer (AssetClass, PubKeyHash)
  | ProvideCollateralRedeemer (AssetClass, PubKeyHash)
  | RevokeCollateralRedeemer (AssetClass, PubKeyHash) AssetClass -- TODO we need to check amountOfCollateralNeededLovelace <= userCollateralBalanceLovelace
    deriving Show

PlutusTx.unstableMakeIsData ''AaveRedeemer
PlutusTx.makeLift ''AaveRedeemer

-- TODO: solve purescript generation issue with type synonyms
type UserConfigId = (AssetClass, PubKeyHash)
type LendingPoolOperator = PubKeyHash
type Oracles = AssocMap.Map AssetClass Integer -- Shows how many lovelaces should be paid for a specific asset

data AaveDatum =
    LendingPoolDatum LendingPoolOperator
  | ReservesDatum AssetClass (AssocMap.Map AssetClass Reserve) -- State token and reserve currency -> reserve map
  | ReserveFundsDatum
  | UserConfigsDatum AssetClass (AssocMap.Map (AssetClass, PubKeyHash) UserConfig) -- State token and UserConfigId -> user config map
  | UserCollateralFundsDatum PubKeyHash AssetClass -- User pub key and aToken asset type
  deriving stock (Show)

PlutusTx.unstableMakeIsData ''AaveDatum
PlutusTx.makeLift ''AaveDatum
Lens.makeClassyPrisms ''AaveDatum

{-# INLINABLE pickUserConfigs #-}
pickUserConfigs :: AaveDatum -> Maybe (AssetClass, AssocMap.Map (AssetClass, PubKeyHash) UserConfig)
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

-- TODO calculate these params
{-# INLINABLE totalCollateralInLovelace #-}
totalCollateralInLovelace ::
     PubKeyHash
  -> AssocMap.Map AssetClass Integer
  -> AssocMap.Map (AssetClass, PubKeyHash) UserConfig
  -> Maybe Integer
totalCollateralInLovelace actor oracles userConfigs =
  foldrM addCollateral 0 $ AssocMap.toList userConfigs
  where
    addCollateral ::
         ((AssetClass, PubKeyHash), UserConfig)
      -> Integer
      -> Maybe Integer
    addCollateral ((asset, user), UserConfig {..}) currentTotal
      | user == actor =
        (\rate -> rate * ucCollateralizedInvestment + currentTotal) <$>
        AssocMap.lookup asset oracles
      | otherwise = Just currentTotal

-- totalDebtInLovelace = 0
-- doesCollateralCoverNewBorrow  =      amountOfCollateralNeededLovelace <= userCollateralBalanceLovelace
--   where
--     userCollateralBalanceLovelace = totalCollateralInLovelace

data AaveScript
instance Scripts.ValidatorTypes AaveScript where
    type instance RedeemerType AaveScript = AaveRedeemer
    type instance DatumType AaveScript = AaveDatum

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
makeAaveValidator aave datum (DepositRedeemer userConfigId) ctx  = trace "DepositRedeemer" $ validateDeposit aave datum ctx userConfigId
makeAaveValidator aave datum (WithdrawRedeemer userConfigId) ctx = trace "WithdrawRedeemer" $ validateWithdraw aave datum ctx userConfigId
makeAaveValidator aave datum (BorrowRedeemer userConfigId) ctx   = trace "BorrowRedeemer" $ validateBorrow aave datum ctx userConfigId
makeAaveValidator aave datum (RepayRedeemer userConfigId) ctx    = trace "RepayRedeemer" $ validateRepay aave datum ctx userConfigId
makeAaveValidator aave datum (ProvideCollateralRedeemer userConfigId) ctx    = trace "ProvideCollateralRedeemer" $ validateProvideCollateral aave datum ctx userConfigId
makeAaveValidator aave datum (RevokeCollateralRedeemer userConfigId aTokenAsset) ctx    = trace "RevokeCollateralRedeemer" $ validateRevokeCollateral aave datum ctx userConfigId aTokenAsset

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

validateDeposit :: Aave -> AaveDatum -> ScriptContext -> (AssetClass, PubKeyHash) -> Bool
validateDeposit aave (UserConfigsDatum stateToken userConfigs) ctx userConfigId =
  traceIfFalse "validateDeposit: User Configs Datum change is not valid" isValidUserConfigsTransformation
  where
    txInfo = scriptContextTxInfo ctx
    (scriptsHash, scriptsDatumHash) = ownHashes ctx
    userConfigsOutputDatumHash =
      findOnlyOneDatumHashByValue (assetClassValue stateToken 1) $ scriptOutputsAt scriptsHash txInfo
    userConfigsOutputDatum ::
         Maybe (AssetClass, AssocMap.Map (AssetClass, PubKeyHash) UserConfig)
    userConfigsOutputDatum =
      userConfigsOutputDatumHash >>= parseDatum txInfo >>= pickUserConfigs

    isValidUserConfigsTransformation :: Bool
    isValidUserConfigsTransformation =
      maybe False checkUserConfigs userConfigsOutputDatum
    checkUserConfigs :: (AssetClass, AssocMap.Map (AssetClass, PubKeyHash) UserConfig) -> Bool
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

validateWithdraw :: Aave -> AaveDatum -> ScriptContext -> (AssetClass, PubKeyHash) -> Bool
validateWithdraw aave (UserConfigsDatum stateToken userConfigs) ctx userConfigId =
  -- TODO add implementation for this case
  traceIfFalse "validateWithdraw: User Configs Datum change is not valid" False
validateWithdraw aave (ReservesDatum stateToken reserves) ctx userConfigId =
  traceIfFalse "validateWithdraw: Reserves Datum change is not valid" $ checkNegativeReservesTransformation stateToken reserves ctx userConfigId

validateWithdraw aave ReserveFundsDatum ctx (reserveId, actor) =
  traceIfFalse "validateWithdraw: Reserve Funds Datum change is not valid" $ checkNegativeFundsTransformation ctx reserveId actor

validateWithdraw _ _ _ _ = trace "validateWithdraw: Lending Pool Datum management is not allowed" False

validateBorrow :: Aave -> AaveDatum -> ScriptContext -> (AssetClass, PubKeyHash) -> Bool
validateBorrow aave (UserConfigsDatum stateToken userConfigs) ctx userConfigId@(reserveId, actor) =
  traceIfFalse "validateBorrow: User Configs Datum change is not valid" isValidUserConfigsTransformation
  where
    txInfo = scriptContextTxInfo ctx
    (scriptsHash, scriptsDatumHash) = ownHashes ctx
    scriptOutputs = scriptOutputsAt scriptsHash txInfo
    userConfigsOutputDatumHash =
      findOnlyOneDatumHashByValue (assetClassValue stateToken 1) scriptOutputs
    userConfigsOutputDatum ::
         Maybe (AssetClass, AssocMap.Map (AssetClass, PubKeyHash) UserConfig)
    userConfigsOutputDatum =
      userConfigsOutputDatumHash >>= parseDatum txInfo >>= pickUserConfigs

    actorSpentValue = valueSpentFrom txInfo actor
    actorRemainderValue = valuePaidTo txInfo actor

    isValidUserConfigsTransformation :: Bool
    isValidUserConfigsTransformation =
      maybe False checkUserConfigs userConfigsOutputDatum
    checkUserConfigs ::
         (AssetClass, AssocMap.Map (AssetClass, PubKeyHash) UserConfig) -> Bool
    checkUserConfigs (newStateToken, newUserConfigs) =
      newStateToken == stateToken &&
      maybe False (checkRedeemerConfig $ AssocMap.lookup userConfigId userConfigs) (AssocMap.lookup userConfigId newUserConfigs)
    checkRedeemerConfig :: Maybe UserConfig -> UserConfig -> Bool
    checkRedeemerConfig oldState newState =
      let debtAmount = (ucDebt newState -) $ maybe 0 ucDebt oldState
          disbursementAmount = assetClassValueOf actorRemainderValue reserveId - assetClassValueOf actorSpentValue reserveId
       in debtAmount == disbursementAmount && debtAmount > 0 && disbursementAmount > 0 &&
          ucCollateralizedInvestment newState == 0 && maybe True ((== 0) . ucCollateralizedInvestment) oldState

validateBorrow aave (ReservesDatum stateToken reserves) ctx userConfigId =
  traceIfFalse "validateBorrow: Reserves Datum change is not valid" $ checkNegativeReservesTransformation stateToken reserves ctx userConfigId

validateBorrow aave ReserveFundsDatum ctx (reserveId, actor) =
  traceIfFalse "validateBorrow: Reserve Funds Datum change is not valid" $ checkNegativeFundsTransformation ctx reserveId actor

validateBorrow _ _ _ _ = trace "validateBorrow: Lending Pool Datum management is not allowed" False

validateRepay :: Aave -> AaveDatum -> ScriptContext -> (AssetClass, PubKeyHash) -> Bool
validateRepay aave (UserConfigsDatum stateToken userConfigs) ctx userConfigId@(reserveId, actor) =
  traceIfFalse "validateRepay: User Configs Datum change is not valid" isValidUserConfigsTransformation
  where
    txInfo = scriptContextTxInfo ctx
    (scriptsHash, scriptsDatumHash) = ownHashes ctx
    scriptOutputs = scriptOutputsAt scriptsHash txInfo
    userConfigsOutputDatumHash =
      findOnlyOneDatumHashByValue (assetClassValue stateToken 1) scriptOutputs
    userConfigsOutputDatum ::
         Maybe (AssetClass, AssocMap.Map (AssetClass, PubKeyHash) UserConfig)
    userConfigsOutputDatum =
      userConfigsOutputDatumHash >>= parseDatum txInfo >>= pickUserConfigs

    actorSpentValue = valueSpentFrom txInfo actor
    actorRemainderValue = valuePaidTo txInfo actor

    isValidUserConfigsTransformation :: Bool
    isValidUserConfigsTransformation =
      maybe False checkUserConfigs userConfigsOutputDatum
    checkUserConfigs :: (AssetClass, AssocMap.Map (AssetClass, PubKeyHash) UserConfig) -> Bool
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

validateProvideCollateral :: Aave -> AaveDatum -> ScriptContext -> (AssetClass, PubKeyHash) -> Bool
validateProvideCollateral aave  (UserConfigsDatum stateToken userConfigs) ctx userConfigId@(reserveId, actor) =
  traceIfFalse "validateProvideCollateral: User Configs Datum change is not valid" isValidUserConfigsTransformation
  where
    txInfo = scriptContextTxInfo ctx
    (scriptsHash, scriptsDatumHash) = ownHashes ctx
    scriptOutputs = scriptOutputsAt scriptsHash txInfo

    userConfigsOutputDatumHash =
      findOnlyOneDatumHashByValue (assetClassValue stateToken 1) scriptOutputs
    userConfigsOutputDatum ::
         Maybe (AssetClass, AssocMap.Map (AssetClass, PubKeyHash) UserConfig)
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
         (AssetClass, AssocMap.Map (AssetClass, PubKeyHash) UserConfig) -> (PubKeyHash, AssetClass) -> Bool
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

validateRevokeCollateral :: Aave -> AaveDatum -> ScriptContext -> (AssetClass, PubKeyHash) -> AssetClass -> Bool
validateRevokeCollateral aave  (UserConfigsDatum stateToken userConfigs) ctx userConfigId@(reserveId, actor) aTokenAsset =
  traceIfFalse "validateRevokeCollateral: User Configs Datum change is not valid" isValidUserConfigsTransformation
  where
    txInfo = scriptContextTxInfo ctx
    (scriptsHash, scriptsDatumHash) = ownHashes ctx
    scriptOutputs = scriptOutputsAt scriptsHash txInfo

    userConfigsOutputDatumHash =
      findOnlyOneDatumHashByValue (assetClassValue stateToken 1) scriptOutputs
    userConfigsOutputDatum ::
         Maybe (AssetClass, AssocMap.Map (AssetClass, PubKeyHash) UserConfig)
    userConfigsOutputDatum =
      userConfigsOutputDatumHash >>= parseDatum txInfo >>= pickUserConfigs

    actorSpentValue = valueSpentFrom txInfo actor
    actorRemainderValue = valuePaidTo txInfo actor

    isValidUserConfigsTransformation :: Bool
    isValidUserConfigsTransformation =
      maybe False checkUserConfigs userConfigsOutputDatum
    checkUserConfigs ::
         (AssetClass, AssocMap.Map (AssetClass, PubKeyHash) UserConfig) -> Bool
    checkUserConfigs (newStateToken, newUserConfigs) =
      newStateToken == stateToken &&
      fromMaybe False (checkRedeemerConfig <$> (AssocMap.lookup userConfigId userConfigs) <*> (AssocMap.lookup userConfigId newUserConfigs))
    checkRedeemerConfig :: UserConfig -> UserConfig -> Bool
    checkRedeemerConfig oldState newState =
      let newInvestmentAmount = ucCollateralizedInvestment newState
          investmentShrinkedBy = ucCollateralizedInvestment oldState - newInvestmentAmount
          disbursementAmount = assetClassValueOf actorRemainderValue aTokenAsset - assetClassValueOf actorSpentValue aTokenAsset
       in investmentShrinkedBy == disbursementAmount && investmentShrinkedBy > 0 && disbursementAmount > 0 && newInvestmentAmount >= 0 &&
          (ucDebt newState == ucDebt oldState)

validateRevokeCollateral aave  (UserCollateralFundsDatum owner aTokenAsset) ctx (reserveId, actor) revokedAsset =
  traceIfFalse "validateRevokeCollateral: UserCollateralFundsDatum change is not valid" $
  owner == actor && revokedAsset == aTokenAsset && checkNegativeFundsTransformation ctx aTokenAsset actor

validateRevokeCollateral _ _ _ _ _ = trace "validateRevokeCollateral: Lending Pool Datum management is not allowed" False

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

checkNegativeReservesTransformation :: AssetClass
  -> AssocMap.Map AssetClass Reserve
  -> ScriptContext
  -> (AssetClass, PubKeyHash)
  -> Bool
checkNegativeReservesTransformation stateToken reserves ctx (reserveId, _) =
      maybe False checkreserves reservesOutputDatum
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

    checkreserves :: (AssetClass, AssocMap.Map AssetClass Reserve) -> Bool
    checkreserves (newStateToken, newReserves) =
      newStateToken == stateToken &&
      maybe
        False
        checkReserveState
        ((,,) <$> remainderValue <*> AssocMap.lookup reserveId reserves <*> AssocMap.lookup reserveId newReserves)
    checkReserveState :: (Value, Reserve, Reserve) -> Bool
    checkReserveState (value, oldState, newState) =
      let fundsAmount = rAmount newState
      in  assetClassValueOf value reserveId == fundsAmount && fundsAmount >= 0 && checkReservesConsistency oldState newState

checkPositiveReservesTransformation :: AssetClass
  -> AssocMap.Map AssetClass Reserve
  -> ScriptContext
  -> (AssetClass, PubKeyHash)
  -> Bool
checkPositiveReservesTransformation stateToken reserves ctx (reserveId, _) = maybe False checkreserves reservesOutputDatum
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

    checkreserves :: (AssetClass, AssocMap.Map AssetClass Reserve) -> Bool
    checkreserves (newStateToken, newReserves) =
      newStateToken == stateToken &&
      maybe
        False
        checkReserveState
        ((,,) <$> investmentValue <*> AssocMap.lookup reserveId reserves <*> AssocMap.lookup reserveId newReserves)
    checkReserveState :: (Value, Reserve, Reserve) -> Bool
    checkReserveState (value, oldState, newState) =
      let fundsChange = rAmount newState - rAmount oldState
      in  assetClassValueOf value reserveId == fundsChange && fundsChange > 0 && checkReservesConsistency oldState newState

checkReservesConsistency :: Reserve -> Reserve -> Bool
checkReservesConsistency oldState newState =
  rCurrency oldState == rCurrency newState &&
  rAToken oldState == rAToken newState &&
  rLiquidityIndex oldState == rLiquidityIndex newState &&
  rCurrentStableBorrowRate oldState == rCurrentStableBorrowRate newState

aaveProtocolName :: TokenName
aaveProtocolName = "Aave"

aaveInstance :: Aave -> Scripts.TypedValidator AaveScript
aaveInstance aave = Scripts.mkTypedValidator @AaveScript
    ($$(PlutusTx.compile [|| makeAaveValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode aave)
     $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @AaveDatum @AaveRedeemer

aaveValidator :: Aave -> Validator
aaveValidator = Scripts.validatorScript . aaveInstance

aaveHash :: Aave -> Ledger.ValidatorHash
aaveHash = UntypedScripts.validatorHash . aaveValidator

aaveAddress :: Aave -> Ledger.Address
aaveAddress = Ledger.scriptAddress . aaveValidator

aave :: CurrencySymbol -> Aave
aave protocol = Aave (assetClass protocol aaveProtocolName)

oneAdaInLovelace :: Integer
oneAdaInLovelace = 1000000
