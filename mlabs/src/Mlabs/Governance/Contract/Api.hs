{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Contract API for the Governance application
module Mlabs.Governance.Contract.Api (
    Deposit(..)
  , Withdraw(..)
  , ProvideRewards(..)
  , QueryBalance(..)
  , GovernanceSchema
  ) where

import PlutusTx.Prelude
import PlutusTx qualified

import GHC.Generics (Generic)
-- import Numeric.Natural (Natural)
import Playground.Contract (FromJSON, ToJSON, ToSchema)
import Plutus.Contract ( type (.\/))
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Value (Value)
import Prelude qualified as Hask

import Mlabs.Plutus.Contract (Call, IsEndpoint(..))

-- TBD mixed withdraw/deposit endpoint

-- since we have split of withdraw/deposit we might want to ensure that
-- the amounts have to be positive by construction, tbd (for now Natural has no ToSchema instance)
newtype Deposit = Deposit Integer
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving newtype (FromJSON, ToJSON)
  deriving anyclass (ToSchema)

PlutusTx.unstableMakeIsData ''Deposit

newtype Withdraw = Withdraw [(PubKeyHash, Integer)]
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving newtype (FromJSON, ToJSON)
  deriving anyclass (ToSchema)

PlutusTx.unstableMakeIsData ''Withdraw

newtype ProvideRewards = ProvideRewards Value
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving newtype (FromJSON, ToJSON)
  deriving anyclass (ToSchema)

newtype QueryBalance = QueryBalance PubKeyHash
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving newtype (FromJSON, ToJSON)
  deriving anyclass (ToSchema)

-- no need to split schemas
type GovernanceSchema =
        Call Deposit
    .\/ Call Withdraw
    .\/ Call ProvideRewards
    .\/ Call QueryBalance

--- endpoint names

instance IsEndpoint Deposit where
  type EndpointSymbol Deposit = "deposit"

instance IsEndpoint Withdraw where
  type EndpointSymbol Withdraw = "withdraw"

instance IsEndpoint ProvideRewards where
  type EndpointSymbol ProvideRewards = "provide-rewards"

instance IsEndpoint QueryBalance where
  type EndpointSymbol QueryBalance = "query-balance"

