-- | Handlers for PAB simulator
module Mlabs.Lending.Contract.Simulator.Handler(
    Sim
  , LendexContracts(..)
  , InitContract
  , runSimulator
) where

import Prelude
import Data.Monoid (Last)
import Control.Monad.IO.Class
import Data.Functor (void)
import Data.Default (Default (def))

import Data.Aeson (ToJSON, FromJSON)
import Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import GHC.Generics
import Control.Monad.Freer.Extras.Log (LogMsg)
import Control.Monad.Freer (Eff, Member, interpret, type (~>))
import Control.Monad.Freer.Error (Error)

import Plutus.Contract
import Plutus.V1.Ledger.Value (CurrencySymbol)
import Plutus.PAB.Effects.Contract (ContractEffect (..))
import Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Monitoring.PABLogMsg (PABMultiAgentMsg (..))
import Plutus.PAB.Simulator (Simulation, SimulatorEffectHandlers)
import Plutus.PAB.Simulator qualified as Simulator
import Plutus.PAB.Types (PABError (..))
import Plutus.PAB.Webserver.Server qualified as PAB.Server

import Mlabs.Lending.Logic.Types (LendexId)
import qualified Mlabs.Lending.Contract.Api as L
import qualified Mlabs.Lending.Contract.Server as L

-- | Shortcut for Simulator monad for NFT case
type Sim a = Simulation (Builtin LendexContracts) a

-- | Lendex schemas
data LendexContracts
  = Init                  -- ^ init wallets
  | User                  -- ^ we read Lendex identifier and instantiate schema for the user actions
  | Oracle                -- ^ price oracle actions
  | Admin                 -- ^ govern actions
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Pretty LendexContracts where
  pretty = viaShow

type InitContract = Contract (Last CurrencySymbol) EmptySchema L.LendexError ()

handleLendexContracts ::
  ( Member (Error PABError) effs
  , Member (LogMsg (PABMultiAgentMsg (Builtin LendexContracts))) effs
  )
  => LendexId
  -> InitContract
  -> ContractEffect (Builtin LendexContracts) ~> Eff effs
handleLendexContracts lendexId initHandler = Builtin.handleBuiltin getSchema getContract
  where
    getSchema = \case
      Init      -> Builtin.endpointsToSchemas @Empty
      User      -> Builtin.endpointsToSchemas @L.UserSchema
      Oracle    -> Builtin.endpointsToSchemas @L.OracleSchema
      Admin     -> Builtin.endpointsToSchemas @L.AdminSchema
    getContract = \case
      Init      -> SomeBuiltin initHandler
      User      -> SomeBuiltin $ L.userEndpoints lendexId
      Oracle    -> SomeBuiltin $ L.oracleEndpoints lendexId
      Admin     -> SomeBuiltin $ L.adminEndpoints lendexId

handlers :: LendexId -> InitContract -> SimulatorEffectHandlers (Builtin LendexContracts)
handlers lid initContract =
  Simulator.mkSimulatorHandlers @(Builtin LendexContracts) def []
    $ interpret (handleLendexContracts lid initContract)

-- | Runs simulator for Lendex
runSimulator :: LendexId -> InitContract -> Sim () -> IO ()
runSimulator lid initContract act = withSimulator (handlers lid initContract) act

withSimulator :: Simulator.SimulatorEffectHandlers (Builtin LendexContracts) -> Simulation (Builtin LendexContracts) () -> IO ()
withSimulator hs act = void $ Simulator.runSimulationWith hs $ do
  Simulator.logString @(Builtin LendexContracts) "Starting PAB webserver. Press enter to exit."
  shutdown <- PAB.Server.startServerDebug
  void $ act
  void $ liftIO getLine
  shutdown

