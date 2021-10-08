-- | Handlers for PAB simulator
module Mlabs.Nft.Contract.Simulator.Handler (
  Sim,
  NftContracts (..),
  runSimulator,
) where

import Prelude

-- FIXME
-- handler related imports commented out with `-- !` to disable compilation warnings
-- ! import Control.Monad.Freer (
--   Eff,
--   Member,
--   interpret,
--   type (~>),
--  )
-- ! import Control.Monad.Freer.Error (Error)
-- ! import Control.Monad.Freer.Extras.Log (LogMsg)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor (void)
-- ! import Data.Monoid (Last)
import Data.OpenApi.Schema qualified as OpenApi
-- ! import Data.Text (Text, pack)
import Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import GHC.Generics (Generic)
-- ! import Plutus.Contract (Contract, mapError)
-- ! import Plutus.PAB.Effects.Contract (ContractEffect (..))
import Plutus.PAB.Effects.Contract.Builtin (Builtin)
-- ! import Plutus.PAB.Monitoring.PABLogMsg (PABMultiAgentMsg (..))
import Plutus.PAB.Simulator (
  Simulation,
  SimulatorEffectHandlers,
 )
import Plutus.PAB.Simulator qualified as Simulator
-- ! import Plutus.PAB.Types (PABError (..))
import Plutus.PAB.Webserver.Server qualified as PAB.Server

import Mlabs.Nft.Contract.Api qualified as Nft
-- ! import Mlabs.Nft.Contract.Server qualified as Nft
import Mlabs.Nft.Logic.Types (NftId)

-- | Shortcut for Simulator monad for NFT case
type Sim a = Simulation (Builtin NftContracts) a

-- | NFT schemas
data NftContracts
  = -- | author can start NFT and provide NftId
    StartNft
  | -- | we read NftId and instantiate schema for the user actions
    User NftId
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty NftContracts where
  pretty = viaShow

-- FIXME
-- related imports commented out to disable compilation warnings
-- handleNftContracts ::
--   ( Member (Error PABError) effs , 
--     Member (LogMsg (PABMultiAgentMsg (Builtin NftContracts))) effs
--   ) =>
--   Nft.StartParams ->
--   ContractEffect (Builtin NftContracts) ~> Eff effs
-- handleNftContracts sp =
-- Builtin.handleBuiltin getSchema getContract
--   where
--     getSchema = \case
--       StartNft -> Builtin.endpointsToSchemas @Nft.AuthorSchema
--       User _ -> Builtin.endpointsToSchemas @Nft.UserSchema
--     getContract = \case
--       StartNft -> SomeBuiltin (startNftContract sp)
--       User nid -> SomeBuiltin (Nft.userEndpoints nid)

-- FIXME
handlers :: Nft.StartParams -> SimulatorEffectHandlers (Builtin NftContracts)
handlers = error "Fix required after Plutus update"
-- handlers sp =
-- Simulator.mkSimulatorHandlers @(Builtin NftContracts) def def $
--   interpret (handleNftContracts sp)

-- FIXME
-- startNftContract :: Nft.StartParams -> Contract (Last NftId) Nft.AuthorSchema Text ()
-- startNftContract startParams = mapError (pack . show) $ Nft.startNft startParams

-- | Runs simulator for NFT
runSimulator :: Nft.StartParams -> Sim () -> IO ()
runSimulator sp = withSimulator (handlers sp)

withSimulator :: Simulator.SimulatorEffectHandlers (Builtin NftContracts) -> Simulation (Builtin NftContracts) () -> IO ()
withSimulator hs act = void $
  Simulator.runSimulationWith hs $ do
    Simulator.logString @(Builtin NftContracts) "Starting PAB webserver. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug
    void act
    void $ liftIO getLine
    shutdown
