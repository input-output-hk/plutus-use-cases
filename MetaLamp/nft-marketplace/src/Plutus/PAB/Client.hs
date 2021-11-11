{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Plutus.PAB.Client where

import           Control.Concurrent              (threadDelay)
import           Control.Monad                   (forever)
import           Data.Aeson                      (FromJSON, ToJSON, toJSON)
import qualified Data.Text                       as Text
import           Plutus.PAB.MarketplaceContracts (MarketplaceContracts)
import           Plutus.PAB.Webserver.Client     (InstanceClient (..),
                                                  PabClient (..), pabClient)
import           Servant.Client                  (ClientEnv, ClientError,
                                                  runClientM)
import           Wallet.Types                    (ContractInstanceId (..))

loopController :: ClientEnv -> ContractInstanceId -> IO ()
loopController env contractId = forever $ do
    threadDelay 2000000
    runController env contractId

runController :: ClientEnv -> ContractInstanceId -> IO ()
runController env contractId = do
    let InstanceClient {..} = (instanceClient (pabClient @MarketplaceContracts @Integer)) contractId
    res <- runClientM (callInstanceEndpoint "collect" (toJSON ())) env
    print res
    pure ()
