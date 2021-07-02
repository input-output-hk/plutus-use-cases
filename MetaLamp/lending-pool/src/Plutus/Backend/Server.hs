{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Plutus.Backend.Server (runServer) where

import           Control.Concurrent.MVar        (modifyMVar_, newMVar, readMVar)
import           Control.Monad.IO.Class         (liftIO)
import           Data.Map                       (insert, (!?))
import           Network.Wai.Handler.Warp       (run)
import qualified Plutus.Backend.ContractStorage as LendingPool
import           Plutus.Backend.Endpoints       (startupActivation, deposit)
import           Plutus.Backend.Types
import           Plutus.ContractStorage         (ContractStorage (..),
                                                 WithContractStorage,
                                                 withContractStorage)
import qualified Plutus.PAB.Simulation          as PAB
import qualified Plutus.PAB.Simulation          as Simulation
import qualified Plutus.PAB.Simulator           as Simulator
import           Servant
import           Servant.API

runServer :: IO ()
runServer = do
    contractStorage <- liftIO $ do
          storage <- newMVar mempty
          pure ContractStorage {
            saveContractIdToStorage = \wallet endpoint instanceId -> liftIO $ do
              modifyMVar_ storage (pure . insert (wallet, endpoint) instanceId)
            ,
            getContractIdFromStorage = \wallet endpoint -> liftIO $ do
              (!? (wallet, endpoint)) <$> readMVar storage
          }
    activateContractsForMockWallets contractStorage
    print "Lending pool backend started on port 8081."
    print "Use Wallet 1, 2 or 3 to request simulation."
    run 8081 $ withContractStorage contractStorage backendApp
    where
      activateContractsForMockWallets contractStorage =
        Simulator.runSimulationWith Simulation.handlers $
          withContractStorage contractStorage $ startupActivation 


type LendingPoolAPI =
    "deposit" :> ReqBody '[JSON] DepositRequest :> Post '[JSON] OperationStatus

server :: WithContractStorage => Server LendingPoolAPI
server = depositHandler

backendApp :: WithContractStorage => Application
backendApp = serve (Proxy @LendingPoolAPI) server

depositHandler :: WithContractStorage => DepositRequest -> Handler OperationStatus
depositHandler req = do
    res <- liftIO $ deposit req
    return res
