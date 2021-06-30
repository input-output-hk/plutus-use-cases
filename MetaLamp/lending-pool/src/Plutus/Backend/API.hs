{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Plutus.Backend.API where

import Servant
import Plutus.PAB.Effects.Contract.Builtin
import qualified Plutus.PAB.Simulator as Simulator

import Plutus.PAB.Simulation
import Plutus.Backend.API.Withdraw

type LendingPoolAPI =
  "health-check" :> Get '[JSON] ()
    :<|> "withdraw" :> ReqBody '[JSON] () :> Post '[JSON] ()

healthCheck :: Simulator.Simulation (Builtin AaveContracts) ()
healthCheck = pure ()

lendingPoolApi = healthCheck :<|> withdrawEndpoint