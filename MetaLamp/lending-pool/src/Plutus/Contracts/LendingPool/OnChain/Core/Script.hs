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
import qualified Plutus.Contracts.Service.Oracle  as Oracle
import           Plutus.V1.Ledger.Value
import qualified PlutusTx
import qualified PlutusTx.AssocMap                as AssocMap
import           PlutusTx.Prelude                 hiding (Semigroup (..),
                                                   unless)
import           Prelude                          (Semigroup (..))
import qualified Prelude

deriving anyclass instance ToSchema Rational

data Reserve = Reserve
    { rCurrency                :: AssetClass, -- reserve id
      rAToken                  :: AssetClass,
      rAmount                  :: Integer,
      rLiquidityIndex          :: Integer,
      rCurrentStableBorrowRate :: Rational,
      rTrustedOracle           :: (CurrencySymbol, PubKeyHash, Integer, AssetClass)
    }
    deriving stock (Prelude.Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''Reserve
PlutusTx.makeLift ''Reserve
Lens.makeClassy_ ''Reserve

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
      ucDebt                     :: Integer,
      ucCollateralizedInvestment :: Integer
    }
    deriving stock (Prelude.Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''UserConfig
PlutusTx.makeLift ''UserConfig
Lens.makeClassy_ ''UserConfig

data AaveRedeemer =
    StartRedeemer
  | DepositRedeemer (AssetClass, PubKeyHash)
  | WithdrawRedeemer (AssetClass, PubKeyHash)
  | BorrowRedeemer (AssetClass, PubKeyHash) [(CurrencySymbol, PubKeyHash, Integer, AssetClass)]
  | RepayRedeemer (AssetClass, PubKeyHash)
  | ProvideCollateralRedeemer (AssetClass, PubKeyHash)
  | RevokeCollateralRedeemer (AssetClass, PubKeyHash) AssetClass [(CurrencySymbol, PubKeyHash, Integer, AssetClass)]
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

data AaveScript
instance Scripts.ValidatorTypes AaveScript where
    type instance RedeemerType AaveScript = AaveRedeemer
    type instance DatumType AaveScript = AaveDatum
