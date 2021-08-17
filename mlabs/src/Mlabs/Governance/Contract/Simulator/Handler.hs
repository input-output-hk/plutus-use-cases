{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Mlabs.Governance.Contract.Simulator.Handler where

import PlutusTx.Prelude
import Prelude (IO, Show)

import Mlabs.Governance.Contract.Api (GovernanceSchema)
import Mlabs.Governance.Contract.Server (governanceEndpoints)
import Mlabs.Governance.Contract.Validation (GovParams (..))

import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (def))
import Data.Monoid (Last)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import GHC.Generics (Generic)

import Control.Monad.Freer (Eff, Member, interpret, type (~>))
import Plutus.Contract (Contract, EmptySchema)
import Plutus.PAB.Core (EffectHandlers)
import Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), endpointsToSchemas, handleBuiltin)
import Plutus.PAB.Simulator (
  Simulation,
  SimulatorContractHandler,
  SimulatorState,
  mkSimulatorHandlers,
  runSimulationWith,
 )
import Plutus.PAB.Types (PABError)

import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Extras.Log (LogMsg)
import Ledger (CurrencySymbol)
import Plutus.PAB.Effects.Contract (ContractEffect (..))
import Plutus.PAB.Monitoring.PABLogMsg (PABMultiAgentMsg (..))

data GovernanceContracts
  = Bootstrap
  | Governance GovParams
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Pretty GovernanceContracts where
  pretty = viaShow

type BootstrapContract = Contract (Last (CurrencySymbol, CurrencySymbol)) EmptySchema Text ()

handleGovernanceContracts ::
  ( Member (Error PABError) effs
  , Member (LogMsg (PABMultiAgentMsg (Builtin GovernanceContracts))) effs
  ) =>
  BootstrapContract ->
  ContractEffect (Builtin GovernanceContracts)
    ~> Eff effs
handleGovernanceContracts bootstrapContract = handleBuiltin getSchema getContract
  where
    getSchema = \case
      Bootstrap -> endpointsToSchemas @EmptySchema
      Governance _ -> endpointsToSchemas @GovernanceSchema
    getContract = \case
      Bootstrap -> SomeBuiltin bootstrapContract
      Governance params -> SomeBuiltin $ governanceEndpoints params

-- | 'EffectHandlers' for running the PAB as a simulator
simulatorHandlers ::
  BootstrapContract ->
  EffectHandlers (Builtin GovernanceContracts) (SimulatorState (Builtin GovernanceContracts))
simulatorHandlers bootstrapContract = mkSimulatorHandlers def def handler
  where
    handler :: SimulatorContractHandler (Builtin GovernanceContracts)
    handler = interpret (handleGovernanceContracts bootstrapContract)

-- | Run the PAB simulator
runSimulation ::
  BootstrapContract ->
  Simulation (Builtin GovernanceContracts) a ->
  IO (Either PABError a)
runSimulation bootstrapContract = runSimulationWith $ simulatorHandlers bootstrapContract
