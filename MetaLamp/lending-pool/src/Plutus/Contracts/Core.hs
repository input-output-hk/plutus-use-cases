{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Plutus.Contracts.Core where

import           Control.Monad                    hiding (fmap)
import qualified Data.ByteString                  as BS
import qualified Data.Map                         as Map
import           Data.Text                        (Text, pack)
import           Data.Void                        (Void)
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

data Reserve = Reserve
    { rCurrency       :: ReserveId,
      rAToken         :: AssetClass,
      rAmount         :: Integer,
      rDebtToken      :: AssetClass,
      rLiquidityIndex :: Integer
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''Reserve
PlutusTx.makeLift ''Reserve

data UserConfig = UserConfig
    { ucAddress           :: PubKeyHash,
      ucReserveId         :: ReserveId,
      ucUsingAsCollateral :: Bool
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''UserConfig
PlutusTx.makeLift ''UserConfig

type Factory = [ReserveId]
data AaveRedeemer =
  CreateReserveRedeemer Reserve
  | UpdateReserveRedeemer
  | CreateUserRedeemer UserConfig
  | UpdateUserRedeemer
  | WithdrawRedeemer
    deriving Show

PlutusTx.unstableMakeIsData ''AaveRedeemer
PlutusTx.makeLift ''AaveRedeemer

data AaveDatum = FactoryDatum Factory | ReserveDatum Reserve | UserConfigDatum UserConfig | DepositDatum deriving stock (Show)

PlutusTx.unstableMakeIsData ''AaveDatum
PlutusTx.makeLift ''AaveDatum

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
makeAaveValidator _ _ (CreateReserveRedeemer _) _ = True
makeAaveValidator _ _ UpdateReserveRedeemer _     = True
makeAaveValidator _ _ (CreateUserRedeemer _) _    = True
makeAaveValidator _ _ UpdateUserRedeemer _        = True
makeAaveValidator _ _ WithdrawRedeemer _          = True
makeAaveValidator _  _  _  _                      = False

aaveProtocolName :: TokenName
aaveProtocolName = "Aave"

aaveInstance :: Aave -> Scripts.ScriptInstance AaveScript
aaveInstance aave = Scripts.validator @AaveScript
    ($$(PlutusTx.compile [|| makeAaveValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode aave)
     $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @AaveDatum @AaveRedeemer

aaveScript :: Aave -> Validator
aaveScript = Scripts.validatorScript . aaveInstance

aaveHash :: Aave -> Ledger.ValidatorHash
aaveHash = Scripts.validatorHash . aaveScript

aaveAddress :: Aave -> Ledger.Address
aaveAddress = Ledger.scriptAddress . aaveScript

aave :: CurrencySymbol -> Aave
aave protocol = Aave (assetClass protocol aaveProtocolName)
