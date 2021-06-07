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
import           Ext.Plutus.Ledger.Contexts       (findDatumHashByValue, findValueByDatumHash)
import           Ledger                           hiding (singleton)
import           Ledger.Constraints               as Constraints
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import qualified Ledger.Scripts                   as Scripts
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

deriving anyclass instance ToSchema AssetClass

newtype Aave = Aave
    { aaveProtocolInst :: AssetClass
    } deriving stock    (Show, Generic)
      deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''Aave

instance Prelude.Eq Aave where
    u == v = aaveProtocolInst u Prelude.== aaveProtocolInst v

instance Prelude.Ord Aave where
    compare u v = Prelude.compare (aaveProtocolInst u) (aaveProtocolInst v)

type ReserveId = AssetClass

deriving anyclass instance ToSchema Rational

data Reserve = Reserve
    { rCurrency                :: ReserveId,
      rAToken                  :: AssetClass,
      rAmount                  :: Integer,
      rDebtToken               :: AssetClass,
      rLiquidityIndex          :: Integer,
      rCurrentStableBorrowRate :: Rational
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''Reserve
PlutusTx.makeLift ''Reserve

type UserConfigId = (ReserveId, PubKeyHash)

data UserConfig = UserConfig
    {
      ucUsingAsCollateral :: Bool,
      ucDebt              :: Maybe Integer
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''UserConfig
PlutusTx.makeLift ''UserConfig

data AaveRedeemer =
    StartRedeemer
  | DepositRedeemer UserConfigId
  | WithdrawRedeemer
  | BorrowRedeemer
  | RepayRedeemer
    deriving Show

PlutusTx.unstableMakeIsData ''AaveRedeemer
PlutusTx.makeLift ''AaveRedeemer

type LendingPoolOperator = PubKeyHash

data AaveDatum =
  LendingPoolDatum LendingPoolOperator
  | ReservesDatum AssetClass (AssocMap.Map ReserveId Reserve)
  | UserConfigsDatum AssetClass (AssocMap.Map UserConfigId UserConfig)
  | DepositDatum
  | WithdrawDatum
  | BorrowDatum
  | RepayDatum
  deriving stock (Show)

PlutusTx.unstableMakeIsData ''AaveDatum
PlutusTx.makeLift ''AaveDatum
Lens.makeClassyPrisms ''AaveDatum

{-# INLINABLE pickUserConfigs #-}
pickUserConfigs :: AaveDatum -> Maybe (AssetClass, AssocMap.Map UserConfigId UserConfig)
pickUserConfigs (UserConfigsDatum stateToken configs) = Just (stateToken, configs)
pickUserConfigs _ = Nothing

{-# INLINABLE pickReserves #-}
pickReserves :: AaveDatum -> Maybe (AssetClass, AssocMap.Map ReserveId Reserve)
pickReserves (ReservesDatum stateToken configs) = Just (stateToken, configs)
pickReserves _ = Nothing

data AaveScript
instance Scripts.ScriptType AaveScript where
    type instance RedeemerType AaveScript = AaveRedeemer
    type instance DatumType AaveScript = AaveDatum

{-# INLINABLE makeAaveValidator #-}
-- Main validator
-- Each state field must have one or more associated actions(Redeemer types),
-- produced on state update, which are then validated here
-- TODO: write validations
makeAaveValidator :: Aave
                   -> AaveDatum
                   -> AaveRedeemer
                   -> ScriptContext
                   -> Bool
makeAaveValidator aave datum StartRedeemer ctx    = trace "StartRedeemer" $ validateStart aave datum ctx
makeAaveValidator aave datum (DepositRedeemer userConfigId) ctx  = trace "DepositRedeemer" $ validateDeposit aave datum ctx userConfigId
makeAaveValidator _ _ WithdrawRedeemer _ = True
makeAaveValidator _ _ BorrowRedeemer _   = True
makeAaveValidator _ _ RepayRedeemer _    = True
-- makeAaveValidator _  _  _  _                          = False

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

validateDeposit :: Aave -> AaveDatum -> ScriptContext -> UserConfigId -> Bool
validateDeposit aave (UserConfigsDatum stateToken userConfigs) ctx userConfigId =
  traceIfFalse "validateDeposit: User Configs Datum change is not valid" isValidUserConfigsTransformation
  where
    txInfo = scriptContextTxInfo ctx
    (scriptsHash, scriptsDatumHash) = ownHashes ctx
    userConfigsOutputDatumHash =
      findDatumHashByValue (assetClassValue stateToken 1) $ scriptOutputsAt scriptsHash txInfo
    userConfigsOutputDatum ::
         Maybe (AssetClass, AssocMap.Map UserConfigId UserConfig)
    userConfigsOutputDatum =
      userConfigsOutputDatumHash >>= (`findDatum` txInfo) >>= (PlutusTx.fromData . getDatum) >>= pickUserConfigs

    isValidUserConfigsTransformation :: Bool
    isValidUserConfigsTransformation =
      maybe False checkUserConfigs userConfigsOutputDatum
    checkUserConfigs ::
         (AssetClass, AssocMap.Map UserConfigId UserConfig) -> Bool
    checkUserConfigs (newStateToken, newUserConfigs) =
      newStateToken == stateToken &&
      maybe False checkRedeemerConfig (AssocMap.lookup userConfigId newUserConfigs)
    checkRedeemerConfig :: UserConfig -> Bool
    checkRedeemerConfig UserConfig{..} = ucUsingAsCollateral

validateDeposit aave (ReservesDatum stateToken reserves) ctx (reserveId, actor) =
  traceIfFalse "validateDeposit: Reserves Datum change is not valid" isValidReservesTransformation
  where
    txInfo = scriptContextTxInfo ctx
    (scriptsHash, scriptsDatumHash) = ownHashes ctx
    scriptOutputs = scriptOutputsAt scriptsHash txInfo

    reservesOutputDatumHash =
      findDatumHashByValue (assetClassValue stateToken 1) scriptOutputs
    reservesOutputDatum ::
         Maybe (AssetClass, AssocMap.Map ReserveId Reserve)
    reservesOutputDatum =
      reservesOutputDatumHash >>= (`findDatum` txInfo) >>= (PlutusTx.fromData . getDatum) >>= pickReserves

    investmentDatumHash = findDatumHash (Datum $ PlutusTx.toData DepositDatum) txInfo
    investmentValue = investmentDatumHash >>= (`findValueByDatumHash` scriptOutputs)

    isValidReservesTransformation :: Bool
    isValidReservesTransformation =
      maybe False checkreserves reservesOutputDatum
    checkreserves :: (AssetClass, AssocMap.Map ReserveId Reserve) -> Bool
    checkreserves (newStateToken, newReserves) =
      newStateToken == stateToken &&
      maybe
        False
        checkReserveState
        ((,,) <$> investmentValue <*> AssocMap.lookup reserveId reserves <*> AssocMap.lookup reserveId newReserves)
    checkReserveState :: (Value, Reserve, Reserve) -> Bool
    checkReserveState (value, oldState, newState) =
      assetClassValueOf value reserveId == (rAmount newState - rAmount oldState)
validateDeposit aave (LendingPoolDatum _) ctx userConfigId = trace "LendingPoolDatum" False
validateDeposit aave DepositDatum ctx userConfigId = trace "DepositDatum" False
validateDeposit aave WithdrawDatum ctx userConfigId = trace "WithdrawDatum" False
validateDeposit aave BorrowDatum ctx userConfigId = trace "BorrowDatum" False
validateDeposit aave RepayDatum ctx userConfigId = trace "RepayDatum" False

aaveProtocolName :: TokenName
aaveProtocolName = "Aave"

aaveInstance :: Aave -> Scripts.ScriptInstance AaveScript
aaveInstance aave = Scripts.validator @AaveScript
    ($$(PlutusTx.compile [|| makeAaveValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode aave)
     $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @AaveDatum @AaveRedeemer

aaveValidator :: Aave -> Validator
aaveValidator = Scripts.validatorScript . aaveInstance

aaveHash :: Aave -> Ledger.ValidatorHash
aaveHash = Scripts.validatorHash . aaveValidator

aaveAddress :: Aave -> Ledger.Address
aaveAddress = Ledger.scriptAddress . aaveValidator

aave :: CurrencySymbol -> Aave
aave protocol = Aave (assetClass protocol aaveProtocolName)
