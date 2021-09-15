{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Middleware.Capability.Config where

import           Conferer
import qualified Conferer.FromConfig         as Internal
import           Conferer.FromConfig.Warp    ()
import qualified Conferer.Source.CLIArgs     as Cli
import qualified Conferer.Source.Dhall       as Dhall
import qualified Conferer.Source.Env         as Env
import           Data.Text                   (unpack)
import           GHC.Generics
import           Middleware.Capability.Error
import qualified Network.Wai.Handler.Warp    as Warp
import           Polysemy
import qualified Polysemy.Internal           as P
import           Servant.Client              (BaseUrl (BaseUrl), Scheme (Http),
                                              parseBaseUrl)

data AppConfig = AppConfig
  { appConfigServer :: Warp.Settings
  , pabUrl          :: BaseUrl
  } deriving (Generic)

instance Conferer.FromConfig AppConfig

instance DefaultConfig AppConfig where
  configDef = AppConfig
    { appConfigServer = Warp.setPort 8080 configDef
    , pabUrl = configDef
    }

instance Conferer.FromConfig BaseUrl where
  fromConfig = Internal.fetchFromConfigWith (parseBaseUrl . unpack)

instance Conferer.DefaultConfig BaseUrl where
  configDef = BaseUrl Http "localhost" 9080 ""

data ConfigLoader m a where
  Load :: ConfigLoader m AppConfig

makeSem ''ConfigLoader

-- | Run "ConfigLoader" using IO
runConfigLoader :: Members [Embed IO, Error AppError] r
  => Sem (ConfigLoader ': r) a
  -> Sem r a
runConfigLoader = interpret $ \case
  Load -> do
    cfg <- sendOrThrow mkAppConfig
    sendOrThrow $ fetch cfg

mkAppConfig :: IO Config
mkAppConfig = mkConfig' []
  [ Cli.fromConfig
  , Env.fromConfig "middleware"
  , Dhall.fromFilePath "config.dhall"
  ]

sendOrThrow :: forall r a . Members [Embed IO, Error AppError] r
            => IO a
            -> Sem r a
sendOrThrow = fromExceptionVia ConfigLoaderError
