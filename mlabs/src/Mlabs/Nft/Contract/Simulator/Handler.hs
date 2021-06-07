-- | Handlers for PAB simulator
module Mlabs.Nft.Contract.Simulator.Handler(
    Sim
  , NftContracts(..)
  , runSimulator
) where

import Prelude
import Data.Monoid (Last)
import Control.Monad.IO.Class
import Data.Functor (void)

import Data.Aeson (ToJSON, FromJSON)
import Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Control.Monad.Freer.Extras.Log (LogMsg)
import Control.Monad.Freer (Eff, Member, interpret, type (~>))
import Control.Monad.Freer.Error (Error)

import Plutus.Contract
import Plutus.PAB.Effects.Contract (ContractEffect (..))
import Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), type (.\\))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Monitoring.PABLogMsg (PABMultiAgentMsg (..))
import Plutus.PAB.Simulator (Simulation, SimulatorEffectHandlers)
import Plutus.PAB.Simulator qualified as Simulator
import Plutus.PAB.Types (PABError (..))
import Plutus.PAB.Webserver.Server qualified as PAB.Server

import Mlabs.Nft.Logic.Types (NftId)
import qualified Mlabs.Nft.Contract.Api as Nft
import qualified Mlabs.Nft.Contract.Server as Nft

-- | Shortcut for Simulator monad for NFT case
type Sim a = Simulation (Builtin NftContracts) a

-- | NFT schemas
data NftContracts
  = StartNft           -- ^ author can start NFT and provide NftId
  | User NftId         -- ^ we read NftId and instanciate schema for the user actions
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Pretty NftContracts where
  pretty = viaShow

handleNftContracts ::
  ( Member (Error PABError) effs
  , Member (LogMsg (PABMultiAgentMsg (Builtin NftContracts))) effs
  )
  => Nft.StartParams
  -> ContractEffect (Builtin NftContracts) ~> Eff effs
handleNftContracts sp = Builtin.handleBuiltin getSchema getContract
  where
    getSchema = \case
      StartNft -> Builtin.endpointsToSchemas @(Nft.AuthorSchema .\\ BlockchainActions)
      User _   -> Builtin.endpointsToSchemas @(Nft.UserSchema   .\\ BlockchainActions)
    getContract = \case
      StartNft  -> SomeBuiltin (startNftContract sp)
      User nid  -> SomeBuiltin (Nft.userEndpoints nid)

handlers :: Nft.StartParams -> SimulatorEffectHandlers (Builtin NftContracts)
handlers sp =
  Simulator.mkSimulatorHandlers @(Builtin NftContracts) []
    $ interpret (handleNftContracts sp)

startNftContract :: Nft.StartParams -> Contract (Last NftId) Nft.AuthorSchema Text ()
startNftContract startParams = mapError (T.pack . show) $ Nft.startNft startParams

-- | Runs simulator for NFT
runSimulator :: Nft.StartParams -> Sim () -> IO ()
runSimulator sp act = withSimulator (handlers sp) act

withSimulator :: Simulator.SimulatorEffectHandlers (Builtin NftContracts) -> Simulation (Builtin NftContracts) () -> IO ()
withSimulator hs act = void $ Simulator.runSimulationWith hs $ do
  Simulator.logString @(Builtin NftContracts) "Starting PAB webserver. Press enter to exit."
  shutdown <- PAB.Server.startServerDebug
  void $ act
  void $ liftIO getLine
  shutdown


