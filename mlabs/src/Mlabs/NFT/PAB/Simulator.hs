module Mlabs.NFT.PAB.Simulator (
  handlers,
  runSimulator,
) where

-- import PlutusTx.Prelude
import Prelude

import Control.Monad (void)
import Control.Monad.Freer
import Control.Monad.IO.Class (MonadIO (..))
import Data.Default (Default (def))

import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (..))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Simulator (SimulatorEffectHandlers, logString)
import Plutus.PAB.Simulator qualified as Simulator
import Plutus.PAB.Webserver.Server qualified as PAB.Server

import Mlabs.NFT.PAB.MarketplaceContract (MarketplaceContracts)

-- | Start PAB simulator for NFT contracts
runSimulator :: IO ()
runSimulator = void $
  Simulator.runSimulationWith handlers $ do
    logString @(Builtin MarketplaceContracts)
      "Starting NFT marketplace simulated PAB webserver. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug
    _ <- liftIO getLine
    shutdown

-- | Simulator handlers for NFT contracts
handlers :: SimulatorEffectHandlers (Builtin MarketplaceContracts)
handlers =
  Simulator.mkSimulatorHandlers def def $
    interpret (contractHandler Builtin.handleBuiltin)
