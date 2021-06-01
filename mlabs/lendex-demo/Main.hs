-- | Console demo for Lendex
module Main where

import Prelude
import GHC.Generics

import Control.Monad.IO.Class
import Data.Functor
import Control.Monad.Freer.Extras.Log (LogMsg)
import PlutusTx.Prelude (ByteString)
import Control.Monad.Freer (Eff, Member, interpret, type (~>))
import Control.Monad.Freer.Error (Error)
import Data.Aeson (Result(..), fromJSON)
import Data.Row (type (.\\))
import Data.Text.Prettyprint.Doc (Pretty (..), viaShow)

import Plutus.PAB.Effects.Contract (ContractEffect (..))
import Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Monitoring.PABLogMsg (PABMultiAgentMsg (..))
import Plutus.PAB.Simulator (Simulation, SimulatorEffectHandlers)
import Plutus.PAB.Simulator qualified as Simulator
import Plutus.PAB.Types (PABError (..))

import Mlabs.Lending.Logic.Types
import qualified Mlabs.Lending.Contract.Lendex as Lending
import qualified Mlabs.Data.Ray as R

import Data.Text (Text)
import Playground.Contract

import Plutus.Contract
import Data.Monoid (Last(..))
import qualified Data.Text as T

import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import Mlabs.System.Console.PrettyLogger
import Mlabs.System.Console.Utils

import Wallet.Emulator.Wallet qualified as Wallet

-- | Shortcut for Simulator monad for NFT case
type Sim a = Simulation (Builtin LendexContracts) a

-- | Lendex schemas
data LendexContracts
  = Init                  -- ^ init wallets
  | StartLendex           -- ^ admin of the platform can start Lendex and provide LendexId
  | User LendexId         -- ^ we read Lendex identifier and instanciate schema for the user actions
  | PriceOracle LendexId  -- ^ price oracle actions
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Pretty LendexContracts where
  pretty = viaShow

-- | Console demo for Lendex with simulator
main :: IO ()
main = withSimulator handlers $ do
  liftIO $ print "Hi Lendex!"
  where
    withSimulator hs act = void $ Simulator.runSimulationWith hs $ do
      Simulator.logString @(Builtin LendexContracts) "Starting Lendex PAB webserver. Press enter to exit."
      shutdown <- PAB.Server.startServerDebug
      void $ act
      void $ liftIO getLine
      shutdown

handleLendexContracts ::
  ( Member (Error PABError) effs
  , Member (LogMsg (PABMultiAgentMsg (Builtin LendexContracts))) effs
  ) =>
  ContractEffect (Builtin LendexContracts)
    ~> Eff effs
handleLendexContracts = Builtin.handleBuiltin getSchema getContract
  where
    getSchema = undefined
    getContract = undefined

handlers :: SimulatorEffectHandlers (Builtin LendexContracts)
handlers =
  Simulator.mkSimulatorHandlers @(Builtin LendexContracts) []
    $ interpret handleLendexContracts





