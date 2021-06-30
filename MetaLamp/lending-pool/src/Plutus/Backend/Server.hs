{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Plutus.Backend.Server where

import Control.Concurrent (forkFinally, forkIO)
import Control.Concurrent.Availability (Availability, available, newToken)
import Control.Concurrent.MVar
import qualified Control.Concurrent.STM as STM
import Control.Monad (void)
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Function ((&))
import Data.Map (insert, (!?))
import qualified Network.Wai.Handler.Warp as Warp
import Plutus.Backend.API
import Plutus.Backend.API.Withdraw
import Plutus.Backend.ContractStorage
import Plutus.PAB.Core (PABAction, PABRunner (..))
import qualified Plutus.PAB.Core as Core
import Plutus.PAB.Effects.Contract.Builtin (Builtin)
import Plutus.PAB.Simulation
import Plutus.PAB.Simulator
import qualified Plutus.PAB.Simulator as Simulator
import Plutus.PAB.Types (PABError)
import Servant

app ::
  WithContractStorage =>
  PABRunner (Builtin AaveContracts) (SimulatorState (Builtin AaveContracts)) ->
  Application
app pabRunner = do
  let apiServer = Servant.hoistServer (Proxy @LendingPoolAPI) (asHandler pabRunner) lendingPoolApi
  Servant.serve (Proxy @LendingPoolAPI) apiServer
  where
    asHandler PABRunner {runPABAction} = Servant.Handler . ExceptT . fmap (first mapError) . runPABAction

    mapError :: PABError -> Servant.ServerError
    mapError e = Servant.err500 {Servant.errBody = J.encode e}

startServer ::
  Int ->
  PABAction
    (Builtin AaveContracts)
    (SimulatorState (Builtin AaveContracts))
    ()
startServer port = do
  availability <- newToken
  simRunner <- Core.pabRunner
  let warpSettings =
        Warp.defaultSettings
          & Warp.setPort port
          & Warp.setBeforeMainLoop
            (available availability)
  contractStorage <- liftIO $ do
    storage <- newMVar mempty
    pure
      ContractStorage
        { saveContractIdToStorage = \wallet endpoint instanceId -> liftIO do
            modifyMVar_
              storage
              (pure . insert (wallet, endpoint) instanceId),
          getContractIdFromStorage = \wallet endpoint -> liftIO do
            (!? (wallet, endpoint)) <$> readMVar storage
        }
  liftIO $
    Warp.runSettings warpSettings $
      withContractStorage contractStorage $
        app simRunner

runBackendServer :: IO ()
runBackendServer = void $
  runSimulationWith handlers $ do
    Simulator.logString @(Builtin AaveContracts) "Serving PAB backend server at 8080 port."
    _ <- activateContracts
    Simulator.logString @(Builtin AaveContracts) "Contract has been initialized. Starting server."
    startServer 8080