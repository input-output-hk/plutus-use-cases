{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Ext.Plutus.PAB.Webserver.Server where

import Cardano.Wallet.Mock.Types (WalletInfo(..))
import           Control.Concurrent.Availability        (Availability,
                                                         available, newToken)
import           Data.Aeson                             (FromJSON, ToJSON)
import           Data.Proxy
import           Ledger.Crypto                          (pubKeyHash)
import qualified Plutus.PAB.Effects.Contract            as Contract
import           Plutus.PAB.Simulator                   (Simulation)
import qualified Plutus.PAB.Simulator                   as Simulator
import           Plutus.PAB.Webserver.API               (API, WSAPI,
                                                         WalletProxy)
import qualified Plutus.PAB.Webserver.Server            as PAB
import           Servant                                (Application,
                                                         Handler (Handler), Raw,
                                                         ServerT, err500,
                                                         errBody, hoistServer,
                                                         serve,
                                                         serveDirectoryFileServer,
                                                         (:<|>) ((:<|>)))
import qualified Servant
import qualified Data.OpenApi.Schema                    as OpenApi
import qualified Network.Wai.Middleware.Cors            as Cors
import qualified Network.Wai.Middleware.Servant.Options as Cors


-- Note: this definition is only to provide options responses
-- WSAPI is websocket api which does not support options requests
type CombinedAPI t =
      API (Contract.ContractDef t) Integer

startServer :: forall t env.
    ( FromJSON (Contract.ContractDef t)
    , ToJSON (Contract.ContractDef t)
    , Contract.PABContract t
    , Servant.MimeUnrender Servant.JSON (Contract.ContractDef t)
    , OpenApi.ToSchema (Contract.ContractDef t)
    )
    => Simulation t (Simulation t ())
startServer = do
    availability <- newToken
    let mkWalletInfo = do
            (wllt, pk) <- Simulator.addWallet
            pure $ WalletInfo{wiWallet = wllt, wiPubKey = pk, wiPubKeyHash = pubKeyHash pk}
    snd <$> PAB.startServer' corsMiddlewares 9080 (Right mkWalletInfo) Nothing availability 30
    where
        corsMiddlewares =
            [ -- a custom CORS policy since 'simpleCors' doesn't support "content-type" header by default
            let policy = Cors.simpleCorsResourcePolicy { Cors.corsRequestHeaders = [ "content-type" ] }
            in Cors.cors (const $ Just policy)
            -- this middleware handles preflight OPTIONS browser requests
            , Cors.provideOptions (Proxy @(API (Contract.ContractDef t) Integer))
            ]