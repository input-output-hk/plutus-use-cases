{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
-- | Main application

module Middleware.App where

import           Colog                          (Message)
import           Colog.Core.IO                  (logStringStdout)
import           Colog.Message                  (Message)
import           Colog.Polysemy                 (Log, log, runLogAction)
import           Colog.Polysemy.Formatting      (Msg, Severity, WithLog,
                                                 addThreadAndTimeToLog, cmap,
                                                 logInfo, logTextStderr,
                                                 logTextStdout, newLogEnv,
                                                 renderThreadTimeMessage)
import           Control.Monad.Except
import           Data.Aeson                     (encode)
import           Data.Function                  ((&))
import           Data.Text                      (Text)
import           Formatting
import           GHC.Stack                      (HasCallStack)
import           Middleware.API
import           Middleware.Capability.CORS     (corsConfig)
import           Middleware.Capability.Config   (AppConfig (pabUrl),
                                                 ConfigLoader, appConfigServer,
                                                 load, runConfigLoader)
import           Middleware.Capability.Error    hiding (Handler, throwError)
import           Middleware.Capability.ReqIdGen (runReqIdGen)
import           Middleware.Capability.Time     (runTime)
import           Middleware.Dex                 (dexServer, runDex)
import qualified Middleware.Dex.Types           as DexTypes
import           Middleware.PabClient           (runPabClient)
import           Network.Wai
import qualified Network.Wai.Handler.Warp       as Warp
import           Polysemy
import           Polysemy.Reader                (runReader)
import           Prelude                        hiding (log)
import           Servant
import           Servant.Polysemy.Client        (runServantClient,
                                                 runServantClientUrl)
import           Servant.Polysemy.Server
import           System.IO                      (stdout)

runApp :: HasCallStack => IO ()
runApp = do
  logEnv <- newLogEnv stdout
  app <- createApp
      & runConfigLoader
      & runError @AppError
      & addThreadAndTimeToLog
      & runLogAction @IO (logTextStderr & cmap (renderThreadTimeMessage logEnv))
      & runM
  print app

createApp :: (WithLog r, Members '[ConfigLoader, Error AppError, Embed IO] r) => Sem r ()
createApp = do
  appConfig <- load
  -- FIXME: Duplicated in runApp
  logEnv <- embed $ newLogEnv stdout
  let pab = pabUrl appConfig
      serverCfg = appConfigServer appConfig
  logInfo (text % shown) "Running on port: " (Warp.getPort serverCfg)
  logInfo (text % shown) "Bind to: "         (Warp.getHost serverCfg)
  let api = Proxy @API
      app = serve api (hoistServer api (runServer pab logEnv) dexServer)
  runWarpServerSettings' @API serverCfg app
  where
    -- | TODO: Handle all errors, add "JSON" body for error messages and improve messages.
    -- | move error mapper to separate package
    handleErrors (Left (ConfigLoaderError id)) = Left $ notFound "Cannot load configuration file"
    handleErrors (Left err)    = Left $ internalServerError "Internal Server Error"
    handleErrors (Right value) = Right value

    notFound :: Text -> ServerError
    notFound msg =
      err404
        { errBody = encode $ DexTypes.Error msg
        , errHeaders = contentTypeJson
        }

    internalServerError :: Text -> ServerError
    internalServerError msg =
      err500
        { errBody = encode $ DexTypes.Error msg
        , errHeaders = contentTypeJson
        }

    contentTypeJson = [("Content-Type", "application/json")]


    liftHandler = Handler . ExceptT . fmap handleErrors

    -- FIXME: We duplicate effect handling with 'runApp' function

    runServer pabUrl logEnv sem = sem
      & runDex
      & runPabClient
      & runError @AppError
      & runTime
      & runReqIdGen
      & runServantClientUrl pabUrl
      & addThreadAndTimeToLog
      & runLogAction @IO (logTextStderr & cmap (renderThreadTimeMessage logEnv))
      & runM
      & liftHandler

-- | Run the given server with these Warp settings.
runWarpServerSettings'
  :: forall api r
   . ( HasServer api '[]
     , Member (Embed IO) r
     )
  => Warp.Settings
  -> Application
  -> Sem r ()
runWarpServerSettings' settings appServer = withLowerToIO $ \lowerToIO finished -> do
  Warp.runSettings settings (corsConfig appServer)
  finished
