{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications      #-}

module Plutus.Backend.Server (runServer) where

import qualified Plutus.PAB.Simulation as PAB
import Network.Wai.Handler.Warp (run)
import Plutus.Backend.ContractStorage (
    ContractStorage(..), WithContractStorage,
    withContractStorage)
import           Control.Concurrent.MVar (newMVar, readMVar, modifyMVar_)
import           Data.Map                            (insert, (!?))
import           Control.Monad.IO.Class              (liftIO)
import Plutus.Backend.Endpoints (deposit)
import Servant.API
import Servant
import Plutus.Backend.Types

runServer :: IO ()
runServer = do
    let shutdown = PAB.runLendingPool
    contractStorage <- liftIO $ do
          storage <- newMVar mempty
          pure ContractStorage {
            saveContractIdToStorage = \wallet endpoint instanceId -> liftIO $ do
              modifyMVar_ storage (pure . insert (wallet, endpoint) instanceId)
            ,
            getContractIdFromStorage = \wallet endpoint -> liftIO $ do
              (!? (wallet, endpoint)) <$> readMVar storage
          }
    run 8081 $ withContractStorage contractStorage backendApp
    shutdown

type LendingPoolAPI = 
    "deposit" :> ReqBody '[JSON] DepositRequest :> Post '[JSON] OperationStatus

server :: WithContractStorage => Server LendingPoolAPI
server = deposit

backendApp :: WithContractStorage => Application
backendApp = serve (Proxy @LendingPoolAPI) server
