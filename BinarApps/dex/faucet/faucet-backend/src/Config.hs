{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Config (loadAppConfig, AppConfig(..), FaucetConfig(..), NodeConfig(..), SocketConfig(..), WalletConfig(..), MintConfig(..)) where

import           GHC.Generics             (Generic)

import qualified Conferer
import           Conferer.FromConfig.Warp ()
import qualified Conferer.Source.CLIArgs  as Cli
import qualified Conferer.Source.Dhall    as Dhall
import qualified Conferer.Source.Env      as Env
import           Data.Aeson               (KeyValue ((.=)), ToJSON (toJSON),
                                           object)
import           Data.Text                (Text)
import qualified Network.Wai.Handler.Warp as Warp

data WalletConfig = WalletConfig {
    walletConfigAddress  :: Text,
    walletConfigSkey     :: FilePath,
    walletConfigLovelace :: Integer
} deriving (Generic, ToJSON)

data MintConfig = MintConfig {
    mintConfigTokens   :: [Text],
    mintConfigQuantity :: Integer
} deriving (Generic, ToJSON)

data FaucetConfig = FaucetConfig {
    faucetConfigMint   :: MintConfig,
    faucetConfigWallet :: WalletConfig
} deriving (Generic, ToJSON)

newtype SocketConfig = SocketConfig {
    socketConfigPath :: FilePath
} deriving (Generic, ToJSON, Show)

data NodeConfig = NodeConfig {
    nodeConfigMagic   :: Maybe String,
    nodeConfigNetwork :: String,
    nodeConfigSocket  :: SocketConfig
} deriving (Generic, ToJSON,Show)

data AppConfig = AppConfig {
    appConfigServer :: Warp.Settings,
    appConfigFaucet :: FaucetConfig,
    appConfigNode   :: NodeConfig
} deriving (Generic)

instance ToJSON AppConfig where
    toJSON cfg = object
      [
        "server" .= object
          [
              "server" .= object
                [
                    "port" .= port cfg
                ]
          ],
        "faucet" .= toJSON (appConfigFaucet cfg),
        "node" .= toJSON (appConfigNode cfg)
      ]
      where
          port = Warp.getPort . appConfigServer

instance Conferer.FromConfig NodeConfig
instance Conferer.FromConfig WalletConfig
instance Conferer.FromConfig FaucetConfig
instance Conferer.FromConfig SocketConfig
instance Conferer.FromConfig MintConfig
instance Conferer.FromConfig AppConfig

instance Conferer.DefaultConfig AppConfig where
  configDef = AppConfig {
      appConfigServer = Warp.setPort 8080 Conferer.configDef,
      appConfigFaucet = FaucetConfig (MintConfig [] 100) (WalletConfig "" "" 100),
      appConfigNode = NodeConfig (Just "1097911063") "" (SocketConfig "")
    }

loadAppConfig :: IO AppConfig
loadAppConfig = do
    config <- mkConfig
    Conferer.fetch config
  where
    mkConfig :: IO Conferer.Config
    mkConfig = Conferer.mkConfig' []
     [ Cli.fromConfig
       , Env.fromConfig "faucet"
       , Dhall.fromFilePath "app.config"
     ]

