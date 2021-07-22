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

module Plutus.Contracts.LendingPool.OnChain.Core.Script where

import           Control.Lens                       ((^?))
import qualified Control.Lens                       as Lens
import           Control.Monad                      hiding (fmap)
import qualified Data.ByteString                    as BS
import qualified Data.Map                           as Map
import           Data.Text                          (Text, pack)
import           Data.Void                          (Void)
import           Ext.Plutus.Ledger.Contexts         (findOnlyOneDatumHashByValue,
                                                     findValueByDatumHash,
                                                     parseDatum, scriptInputsAt,
                                                     valueSpentFrom)
import           Ledger                             hiding (singleton)
import           Ledger.Constraints                 as Constraints
import           Ledger.Constraints.OnChain         as Constraints
import           Ledger.Constraints.TxConstraints   as Constraints
import qualified Ledger.Scripts                     as UntypedScripts
import qualified Ledger.Typed.Scripts               as Scripts
import           Playground.Contract
import           Plutus.Abstract.IncentivizedAmount (IncentivizedAmount (..))
import qualified Plutus.Abstract.State              as State
import           Plutus.Contract                    hiding (when)
import qualified Plutus.Contracts.Service.Oracle    as Oracle
import           Plutus.V1.Ledger.Value
import qualified PlutusTx
import qualified PlutusTx.AssocMap                  as AssocMap
import           PlutusTx.Prelude                   hiding (Semigroup (..),
                                                     unless)
import           Prelude                            (Semigroup (..))
import qualified Prelude

newtype Aave = Aave
    { aaveProtocolInst :: AssetClass
    } deriving stock    (Prelude.Eq, Show, Generic)
      deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''Aave

data Reserve = Reserve
    { rCurrency                :: AssetClass, -- reserve id
      rAToken                  :: AssetClass,
      rAmount                  :: Integer,
      rLiquidityIndex          :: Integer,
      rCurrentStableAccrualRate :: Rational,
      rCurrentStableBorrowRate :: Rational,
      rTrustedOracle           :: (CurrencySymbol, PubKeyHash, Integer, AssetClass)
    }
    deriving stock (Prelude.Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- seems like the only way to define PlutusTx's Eq for data that doesn't break validators
instance Eq Reserve where
  a == b =
    rCurrency a == rCurrency b && rAToken a == rAToken b &&
    rAmount a == rAmount b && rLiquidityIndex a == rLiquidityIndex b
    && rCurrentStableBorrowRate a == rCurrentStableBorrowRate b && rTrustedOracle a == rTrustedOracle b

instance Eq (CurrencySymbol, PubKeyHash, Integer, AssetClass) where
  (a1, b1, c1, d1) == (a2, b2, c2, d2) = a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2

PlutusTx.unstableMakeIsData ''Reserve
PlutusTx.makeLift ''Reserve
Lens.makeClassy_ ''Reserve

deriving anyclass instance ToSchema IncentivizedAmount

PlutusTx.unstableMakeIsData ''IncentivizedAmount
PlutusTx.makeLift ''IncentivizedAmount

data UserConfig = UserConfig
    {
      ucDebt                     :: IncentivizedAmount,
      ucCollateralizedInvestment :: IncentivizedAmount
    }
    deriving stock (Prelude.Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Eq UserConfig where
  a == b = ucDebt a == ucDebt b && ucCollateralizedInvestment a == ucCollateralizedInvestment b

PlutusTx.unstableMakeIsData ''UserConfig
PlutusTx.makeLift ''UserConfig
Lens.makeClassy_ ''UserConfig

type UserConfigId = (AssetClass, PubKeyHash)

data AaveRedeemer =
    StartRedeemer
  | DepositRedeemer UserConfigId
  | WithdrawRedeemer UserConfigId
  | BorrowRedeemer UserConfigId [(CurrencySymbol, PubKeyHash, Integer, AssetClass)] Slot
  | RepayRedeemer UserConfigId Slot
  | ProvideCollateralRedeemer UserConfigId
  | RevokeCollateralRedeemer UserConfigId AssetClass [(CurrencySymbol, PubKeyHash, Integer, AssetClass)] Slot
    deriving Show

PlutusTx.unstableMakeIsData ''AaveRedeemer
PlutusTx.makeLift ''AaveRedeemer

type LendingPoolOperator = PubKeyHash

type Oracles = AssocMap.Map AssetClass Integer -- Shows how many lovelaces should be paid for a specific asset

data AaveState = AaveState { asReserves :: AssetClass, asUserConfigs :: AssetClass }
    deriving stock (Prelude.Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Eq AaveState where
  a == b = asReserves a == asReserves b && asUserConfigs a == asUserConfigs b

PlutusTx.unstableMakeIsData ''AaveState
PlutusTx.makeLift ''AaveState

data AaveDatum =
    LendingPoolDatum LendingPoolOperator
  | ReservesDatum AaveState (AssocMap.Map AssetClass Reserve) -- State token and reserve currency -> reserve map
  | ReserveFundsDatum
  | UserConfigsDatum AaveState (AssocMap.Map UserConfigId UserConfig) -- State token and UserConfigId -> user config map
  | UserCollateralFundsDatum PubKeyHash AssetClass -- User pub key and aToken asset type
  deriving stock (Show)

PlutusTx.unstableMakeIsData ''AaveDatum
PlutusTx.makeLift ''AaveDatum
Lens.makeClassyPrisms ''AaveDatum

data AaveScript
instance Scripts.ValidatorTypes AaveScript where
    type instance RedeemerType AaveScript = AaveRedeemer
    type instance DatumType AaveScript = AaveDatum
