{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fobject-code #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

-- | Contract API for the Governance application
module Mlabs.Governance.Contract.Api (
    StartGovernance(..)
  , Deposit(..)
  , Withdraw(..)
  , ProvideRewards(..)
  , QueryBalance(..)
  , GovernanceSchema
  , AssetClassNft(..)
  , AssetClassGov(..)
  ) where

import PlutusTx.Prelude
import PlutusTx qualified

import GHC.Generics (Generic)
-- import Numeric.Natural (Natural)
import Playground.Contract (FromJSON, ToJSON, ToSchema)
import Plutus.Contract ( type (.\/), BlockchainActions )
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Value (Value, CurrencySymbol, TokenName)
import Prelude qualified as Hask

import Mlabs.Plutus.Contract (Call, IsEndpoint(..))

-- TODO: Once AssetClass has a ToSchema instance, change this to a newtype.
--       or not. this is fine really. 
data AssetClassNft = AssetClassNft {
    acNftCurrencySymbol :: !CurrencySymbol
  , acNftTokenName :: !TokenName
  } deriving (Hask.Show, Hask.Eq, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''AssetClassNft
PlutusTx.makeLift ''AssetClassNft

data AssetClassGov = AssetClassGov {
    acGovCurrencySymbol :: !CurrencySymbol
  , acGovTokenName :: !TokenName
  } deriving (Hask.Show, Hask.Eq, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''AssetClassGov
PlutusTx.makeLift ''AssetClassGov

data StartGovernance = StartGovernance {
    sgNft :: !AssetClassNft
  , sgGov :: !AssetClassGov
  } deriving stock (Show, Generic, Hask.Eq)
    deriving anyclass (FromJSON, ToJSON, ToSchema)  

-- since we have split of withdraw/deposit we might want to ensure that
-- the amounts have to be positive by construction, tbd (for now Natural has no ToSchema instance)
newtype Deposit = Deposit Integer
  deriving stock (Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

PlutusTx.unstableMakeIsData ''Deposit

newtype Withdraw = Withdraw Value
  deriving stock (Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

PlutusTx.unstableMakeIsData ''Withdraw

newtype ProvideRewards = ProvideRewards Value
  deriving stock (Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- may be deprecated/decided on the other way of determining vote weight.
-- see the slack discussion, for take care of this last
newtype QueryBalance = QueryBalance PubKeyHash
  deriving stock (Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- no need to split schemas
type GovernanceSchema =
  BlockchainActions
    .\/ Call StartGovernance
    .\/ Call Deposit
    .\/ Call Withdraw
    .\/ Call ProvideRewards
    .\/ Call QueryBalance

--- endpoint names

instance IsEndpoint StartGovernance where
  type EndpointSymbol StartGovernance = "start-governance"

instance IsEndpoint Deposit where
  type EndpointSymbol Deposit = "deposit"

instance IsEndpoint Withdraw where
  type EndpointSymbol Withdraw = "withdraw"

instance IsEndpoint ProvideRewards where
  type EndpointSymbol ProvideRewards = "provide-rewards"

instance IsEndpoint QueryBalance where
  type EndpointSymbol QueryBalance = "query-balance"

