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
import           Ext.Plutus.Ledger.Contexts       (findDatumHashByValue)
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
  | DepositRedeemer
  | WithdrawRedeemer
  | BorrowRedeemer
  | RepayRedeemer
    deriving Show

PlutusTx.unstableMakeIsData ''AaveRedeemer
PlutusTx.makeLift ''AaveRedeemer

type LendingPoolOperator = PubKeyHash

data AaveDatum =
  LendingPoolDatum LendingPoolOperator
  | ReservesDatum (AssocMap.Map ReserveId Reserve)
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
pickUserConfigs :: AaveDatum -> Maybe (AssocMap.Map UserConfigId UserConfig)
pickUserConfigs (UserConfigsDatum _ configs) = Just configs
pickUserConfigs _ = Nothing

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
makeAaveValidator aave datum DepositRedeemer ctx  = trace "DepositRedeemer" $ validateDeposit aave datum ctx
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

validateDeposit :: Aave -> AaveDatum -> ScriptContext -> Bool
validateDeposit aave (UserConfigsDatum stateToken userConfigs) ctx = trace "UserConfigsDatum" $ isNothing userConfigsOutputDatum
  where
    txInfo = scriptContextTxInfo ctx
    (scriptsHash, scriptsDatumHash) = ownHashes ctx
    userConfigsOutputDatumHash = findDatumHashByValue (assetClassValue stateToken 1) $ scriptOutputsAt scriptsHash txInfo
    userConfigsOutputDatum :: Maybe (AssocMap.Map UserConfigId UserConfig)
    userConfigsOutputDatum = userConfigsOutputDatumHash >>= (`findDatum` txInfo) >>= (PlutusTx.fromData . getDatum) >>= pickUserConfigs
validateDeposit aave (LendingPoolDatum _) ctx = trace "LendingPoolDatum" False
validateDeposit aave (ReservesDatum _) ctx = trace "ReservesDatum" False
validateDeposit aave DepositDatum ctx = trace "DepositDatum" False
validateDeposit aave WithdrawDatum ctx = trace "WithdrawDatum" False
validateDeposit aave BorrowDatum ctx = trace "BorrowDatum" False
validateDeposit aave RepayDatum ctx = trace "RepayDatum" False

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
