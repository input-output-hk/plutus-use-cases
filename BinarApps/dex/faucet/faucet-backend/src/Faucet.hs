{-# LANGUAGE OverloadedStrings #-}

module Faucet where

import           Config
import           Control.Concurrent          (newMVar)
import           Control.Exception           (throw)
import           Control.Monad               (unless)
import           Control.Monad.Reader        (ReaderT (runReaderT))
import           Data.Aeson.Encode.Pretty    (encodePretty)
import qualified Data.ByteString.Lazy.Char8  as BLC
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import qualified Data.Text                   as T
import           Faucet.API
import           Faucet.Data
import           Faucet.Internal
import           Network.Wai.Handler.Warp    (getHost, getPort, runSettings)
import           Network.Wai.Middleware.Cors (simpleCors)
import           Servant                     ()

handleFaucet'    :: AppFaucetContext -> Set TokenName -> AddressParam -> Maybe TokenName -> IO ()
handleFaucet' ctx availableTokens address tn = do
  tn' <- liftMaybeIO tn $ LackOfTokenNameError $ show address
  unless (Set.member tn' availableTokens) $ throw (TokenNameNotSupportedError tn')
  putStrLn $ "Sending " <> show tn' <> " to " <> show address
  runReaderT (faucet address tn') ctx

faucetApp :: IO ()
faucetApp = do
    config <- loadAppConfig
    putStrLn $ "Starting server on " <> hostPort config <> " ..."
    BLC.putStrLn $ "Config: " <> encodePretty config
    faucetContexct <- mkFaucetContext config
    let availableTokens = TokenName . T.unpack <$> (mintConfigTokens . faucetConfigMint . appConfigFaucet) config
    runSettings (appConfigServer config) $
      simpleCors $ app FaucetService {
        getTokens =  availableTokens,
        handleFaucet = handleFaucet' faucetContexct (Set.fromList availableTokens)
      }
  where
      port = getPort . appConfigServer
      host  = getHost . appConfigServer
      hostPort cfg = foldl (<>) "" [show $ host cfg, ":", show $ port cfg]
      mkFaucetContext :: AppConfig -> IO AppFaucetContext
      mkFaucetContext (AppConfig _ faucetCfg nodeCfg) = do
        refs <- newMVar Set.empty
        return $ AppFaucetContext nodeCfg faucetCfg refs
