{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RecordWildCards         #-}

module Plutus.PAB.Client where

import Servant.Client (runClientM, ClientError, ClientEnv)
import Plutus.PAB.Webserver.Client (pabClient, PabClient(..), InstanceClient(..))
import           Wallet.Types                                   (ContractInstanceId(..))
import Data.Aeson (toJSON, ToJSON, FromJSON)
import qualified Data.Text as Text
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Plutus.PAB.MarketplaceContracts (MarketplaceContracts)

loopController :: ClientEnv -> ContractInstanceId -> IO ()
loopController env contractId = forever $ do
    threadDelay 2000000
    runController env contractId

runController :: ClientEnv -> ContractInstanceId -> IO ()
runController env (ContractInstanceId contractId) = do
    let InstanceClient {..} = (instanceClient (pabClient @MarketplaceContracts @Integer)) (Text.pack . show $ contractId)
    res <- runClientM (callInstanceEndpoint "collect" (toJSON ())) env
    print res
    pure ()
