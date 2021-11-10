-- | Handlers for PAB simulator
module Mlabs.Lending.Contract.Simulator.Handler (
  Sim,
  LendexContracts (..),
  InitContract,
  runSimulator,
) where

import Prelude

-- handler related imports commented out with `-- !` to disable compilation warnings
-- ! import Control.Monad.Freer (
--   Eff,
--   Member,
--   interpret,
--   type (~>),
--  )
-- ! import Control.Monad.Freer.Error (Error)
-- ! import Control.Monad.Freer.Extras.Log (LogMsg)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON)

-- ! import Data.Default (Default (def))
import Data.Functor (void)
import Data.Monoid (Last)
import Data.OpenApi.Schema qualified as OpenApi
import Prettyprinter (Pretty (..), viaShow)
import GHC.Generics (Generic)
import Plutus.Contract (Contract, EmptySchema)

-- ! import Plutus.PAB.Effects.Contract (ContractEffect (..))
import Plutus.PAB.Effects.Contract.Builtin (
  Builtin,
  -- !  , SomeBuiltin (..)
 )

-- ! import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
-- ! import Plutus.PAB.Monitoring.PABLogMsg (PABMultiAgentMsg (..))
import Plutus.PAB.Simulator (
  Simulation,
  SimulatorEffectHandlers,
 )
import Plutus.PAB.Simulator qualified as Simulator

-- ! import Plutus.PAB.Types (PABError (..))
import Plutus.PAB.Webserver.Server qualified as PAB.Server
import Plutus.V1.Ledger.Value (CurrencySymbol)

-- ! import Mlabs.Lending.Contract.Api qualified as Api
import Mlabs.Lending.Contract.Server qualified as Server
import Mlabs.Lending.Logic.Types (LendexId)

-- | Shortcut for Simulator monad for NFT case
type Sim a = Simulation (Builtin LendexContracts) a

-- | Lendex schemas
data LendexContracts
  = -- | init wallets
    Init
  | -- | we read Lendex identifier and instantiate schema for the user actions
    User
  | -- | price oracle actions
    Oracle
  | -- | govern actions
    Admin
  | -- | Query actions
    Query
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty LendexContracts where
  pretty = viaShow

type InitContract = Contract (Last CurrencySymbol) EmptySchema Server.LendexError ()

-- FIXME
-- handleLendexContracts lendexId initHandler =
-- handleLendexContracts ::
--   ( Member (Error PABError) effs
--   , Member (LogMsg (PABMultiAgentMsg (Builtin LendexContracts))) effs
--   ) =>
--   LendexId ->
--   InitContract ->
--   ContractEffect (Builtin LendexContracts) ~> Eff effs
-- handleLendexContracts lendexId initHandler =
-- handleLendexContracts lendexId initHandler =
-- Builtin.handleBuiltin getSchema getContract
-- where
--   getSchema = \case
--     Init -> Builtin.endpointsToSchemas @EmptySchema
--     User -> Builtin.endpointsToSchemas @Api.UserSchema
--     Oracle -> Builtin.endpointsToSchemas @Api.OracleSchema
--     Admin -> Builtin.endpointsToSchemas @Api.AdminSchema
--     Query -> Builtin.endpointsToSchemas @Api.QuerySchema
--   getContract = \case
--     Init -> SomeBuiltin initHandler
--     User -> SomeBuiltin $ Server.userEndpoints lendexId
--     Oracle -> SomeBuiltin $ Server.oracleEndpoints lendexId
--     Admin -> SomeBuiltin $ Server.adminEndpoints lendexId
--     Query -> SomeBuiltin $ Server.queryEndpoints lendexId

-- FIXME
handlers :: LendexId -> InitContract -> SimulatorEffectHandlers (Builtin LendexContracts)
handlers = error "Fix required after Plutus update"

-- handlers lid initContract =
-- Simulator.mkSimulatorHandlers @(Builtin LendexContracts) def [] $
--   interpret (handleLendexContracts lid initContract)

-- | Runs simulator for Lendex
runSimulator :: LendexId -> InitContract -> Sim () -> IO ()
runSimulator lid initContract = withSimulator (handlers lid initContract)

withSimulator :: Simulator.SimulatorEffectHandlers (Builtin LendexContracts) -> Simulation (Builtin LendexContracts) () -> IO ()
withSimulator hs act = void $
  Simulator.runSimulationWith hs $ do
    Simulator.logString @(Builtin LendexContracts) "Starting PAB webserver. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug
    void act
    void $ liftIO getLine
    shutdown
