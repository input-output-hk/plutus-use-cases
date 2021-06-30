module Plutus.Backend.API.Withdraw where

import Plutus.PAB.Effects.Contract.Builtin
import Plutus.PAB.Simulation
import qualified Plutus.PAB.Simulator as Simulator

withdrawEndpoint :: () -> Simulator.Simulation (Builtin AaveContracts) ()
withdrawEndpoint req = do
  pure ()
