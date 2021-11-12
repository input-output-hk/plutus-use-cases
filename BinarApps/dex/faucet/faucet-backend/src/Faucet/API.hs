{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Faucet.API (app, FaucetService(..)) where

import           Control.Exception    (Exception, try)
import           Control.Monad.Except (ExceptT (ExceptT), MonadIO (liftIO),
                                       (<=<))
import           Data.Aeson           (ToJSON (toJSON), encode)
import           Data.Text            (Text, pack)
import           Faucet.Data          (AddressParam, FaucetException (..),
                                       TokenName (..))
import           GHC.Generics         (Generic)
import           Servant

type API = "faucet" :> Capture "address" AddressParam :> QueryParam "tokenName" TokenName :> GetNoContent
            :<|> "tokens" :> Get '[JSON] [TokenName]

newtype ErrorMessage = ErrorMessage
  { errorMessage :: Text
  } deriving (Generic, ToJSON)

server :: FaucetService -> Server API
server service = handleFaucet' :<|> handleTokens
  where
      handleFaucet' :: AddressParam -> Maybe TokenName -> Handler NoContent
      handleFaucet' address tn = liftHandler $ handleFaucet service address tn >> return NoContent
      handleTokens :: Handler [TokenName]
      handleTokens = liftHandler $ return $ getTokens service

      adaptErrors ::  Either FaucetException a -> Either ServerError a
      adaptErrors (Left (TokenNameNotSupportedError (TokenName tn))) = Left $ notFound ("Token not supported: '" <> pack tn <> "'")
      adaptErrors (Left (LackOfTokenNameError _)) = Left $ badRequest (pack "Lack of 'tokens' parameter")
      adaptErrors (Left NoUtxoToConsumeError) = Left $ conflict (pack "Wait few seconds and try again")
      adaptErrors (Left _) = Left $ internalServerError "Internal Server Error"
      adaptErrors (Right value) = Right value

      conflict = httpError err409

      internalServerError = httpError err500

      notFound = httpError err404

      badRequest = httpError err400

      httpError :: ServerError -> Text -> ServerError
      httpError err msg =
        err
          { errBody = encode $ toJSON $ ErrorMessage msg
          , errHeaders = errorHeaders
          }
        where
          errorHeaders = [("Content-Type", "application/json")]

      liftHandler :: IO a -> Handler a
      liftHandler = Handler . ExceptT . fmap adaptErrors . (logError <=< try)
        where
          logError :: Exception e =>  Either e a -> IO (Either e a)
          logError err@(Left e) = do
            liftIO $ putStrLn $ "Error: " <> show e
            return err
          logError a            = return a

proxyAPI :: Proxy API
proxyAPI = Proxy

data FaucetService = FaucetService {
  getTokens    :: [TokenName],
  handleFaucet :: AddressParam -> Maybe TokenName -> IO ()
}

app :: FaucetService -> Application
app = serve proxyAPI . server
