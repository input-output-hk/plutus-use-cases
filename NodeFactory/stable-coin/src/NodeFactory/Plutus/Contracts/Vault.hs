{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module NodeFactory.Plutus.Contracts.Vault
    () where

import           Control.Monad             hiding (fmap)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Map                  as Map
import           Data.Monoid               (Last (..))
import           Data.Text                 (Text, pack)
import           GHC.Generics              (Generic)
import           Plutus.Contract           as Contract hiding (when)
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup(..), unless)
import           Ledger                    hiding (singleton)
import           Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Ledger.Ada                as Ada
import           Plutus.Contracts.Currency as Currency
import           Prelude                   (Semigroup (..))
import qualified Prelude                   as Prelude

data Vault = Vault
    { owner  :: !PubKeyHash      -- owner of the of the vault
    , amount :: !Integer         -- amount of ADA in vault
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''Vault

data VaultRedeemer = Create | Close  -- TODO - add liquidation
    deriving Show

PlutusTx.unstableMakeIsData ''VaultRedeemer

-- 

{-# INLINABLE mkVaultValidator #-} -- validator functions
mkVaultValidator :: Vault -> () -> VaultRedeemer -> ScriptContext -> Bool
mkVaultValidator oracle x r ctx =
    traceIfFalse "check vault constraints" True  -- TODO - add validation logic

-- 

data Vaulting
instance Scripts.ScriptType Vaulting where
    type instance DatumType Vaulting = ()
    type instance RedeemerType Vaulting = VaultRedeemer

vaultInst :: Vault -> Scripts.ScriptInstance Vaulting
vaultInst oracle = Scripts.validator @Vaulting
    ($$(PlutusTx.compile [|| mkVaultValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode oracle)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @VaultRedeemer

vaultValidator :: Vault -> Validator
vaultValidator = Scripts.validatorScript . vaultInst

vaultAddress :: Vault -> Ledger.Address
vaultAddress = scriptAddress . vaultValidator
