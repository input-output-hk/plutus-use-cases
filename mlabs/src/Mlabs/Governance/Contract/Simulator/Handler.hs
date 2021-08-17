
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Mlabs.Governance.Contract.Simulator.Handler where

import PlutusTx.Prelude
import Prelude (Show, IO)

import Mlabs.Governance.Contract.Api (GovernanceSchema)
import Mlabs.Governance.Contract.Validation (AssetClassGov(..))
import Mlabs.Governance.Contract.Server (governanceEndpoints)

import           Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import           Data.Aeson                (FromJSON, ToJSON)
import           GHC.Generics              (Generic)
import           Data.Default                             (Default (def))
import           Data.Text   (Text)
import Data.Monoid (Last)

import           Control.Monad.Freer                      (interpret, Eff, Member, type (~>))
import           Data.Default                             ()
import           Plutus.PAB.Core                          (EffectHandlers)
import           Plutus.PAB.Effects.Contract.Builtin      (Builtin, SomeBuiltin (..), handleBuiltin, endpointsToSchemas)
import           Plutus.PAB.Simulator                     (Simulation, SimulatorContractHandler, SimulatorState,
                                                           mkSimulatorHandlers, runSimulationWith)
import           Plutus.PAB.Types                         (PABError)
import           Plutus.Contract                          (Contract, EmptySchema)

import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Extras.Log (LogMsg)
import Plutus.PAB.Monitoring.PABLogMsg (PABMultiAgentMsg (..))
import Plutus.PAB.Effects.Contract (ContractEffect (..))
import Ledger (CurrencySymbol)


-- todo Additional Init contract TBD
data GovernanceContracts 
  = Bootstrap 
  | Governance AssetClassGov
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
handleGovernanceContracts bootstrapContract = handleBuiltin getSchema getContract where
    getSchema = \case
        Bootstrap         -> endpointsToSchemas @EmptySchema
        Governance _      -> endpointsToSchemas @GovernanceSchema
    getContract = \case
        Bootstrap         -> SomeBuiltin   bootstrapContract
        Governance params -> SomeBuiltin $ governanceEndpoints params

-- | 'EffectHandlers' for running the PAB as a simulator
simulatorHandlers ::
  BootstrapContract ->
  EffectHandlers (Builtin GovernanceContracts) (SimulatorState (Builtin GovernanceContracts))
simulatorHandlers bootstrapContract = mkSimulatorHandlers def def handler where
    handler :: SimulatorContractHandler (Builtin GovernanceContracts)
    handler = interpret (handleGovernanceContracts bootstrapContract)

-- | Run the PAB simulator
runSimulation :: 
  BootstrapContract ->
  Simulation (Builtin GovernanceContracts) a -> IO (Either PABError a)
runSimulation bootstrapContract = runSimulationWith $ simulatorHandlers bootstrapContract
