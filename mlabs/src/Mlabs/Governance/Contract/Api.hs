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

import GHC.Generics (Generic)
-- import Numeric.Natural (Natural)
import Playground.Contract (FromJSON, ToJSON, ToSchema)
import Plutus.Contract ( type (.\/), BlockchainActions )
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Value (Value)
import Prelude qualified as Hask

import Mlabs.Plutus.Contract (Call, IsEndpoint(..))

-- since we have split of withdraw/deposit we might want to ensure that
-- the amounts have to be positive by construction, tbd (for now Natural has no ToSchema instance)
newtype Deposit = Deposit Integer
  deriving stock (Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype Withdraw = Withdraw Value
  deriving stock (Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

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
    .\/ Call Deposit
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
