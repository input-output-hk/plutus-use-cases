{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Plutus.Backend.Server (runServer) where

import           Control.Concurrent.MVar        (modifyMVar_, newMVar, readMVar)
import           Control.Monad.IO.Class         (liftIO)
import           Data.Map                       (insert, (!?))
import           Network.Wai.Handler.Warp       (run)
import           Plutus.Backend.ContractStorage (ContractStorage (..),
                                                 WithContractStorage,
                                                 withContractStorage)
import           Plutus.Backend.Endpoints       (deposit)
import           Plutus.Backend.Types
import qualified Plutus.PAB.Simulation          as PAB
import           Servant
import           Servant.API

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
server = depositHandler

backendApp :: WithContractStorage => Application
backendApp = serve (Proxy @LendingPoolAPI) server

depositHandler :: WithContractStorage => DepositRequest -> Handler OperationStatus
depositHandler req = do
    res <- liftIO $ deposit req
    toServerErr res

toServerErr :: WithContractStorage => OperationStatus -> Handler OperationStatus
toServerErr (FailOperation _) = throwError err500
toServerErr SuccessOperation  = return SuccessOperation


