{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.RequestHandler where

import Common.App
import Control.Monad.IO.Class
import Data.Functor.Identity
import Data.Pool
import Database.Groundhog.Postgresql
import Rhyolite.Account
import Rhyolite.Api
import Rhyolite.Backend.Account
import Rhyolite.Backend.App
import Rhyolite.Backend.DB
import Rhyolite.Backend.Logging
import Rhyolite.Backend.Sign
import qualified Web.ClientSession as CS

handleRequests
  :: ( MonadIO m
     , MonadBaseNoPureAborts IO m
     )
  =>  LoggingEnv -> CS.Key -> Pool Postgresql -> RequestHandler DefApp m
handleRequests logger csk db = RequestHandler $ \req -> runLoggingEnv logger $ runDb (Identity db) $ case req of
  ApiRequest_Public r -> case r of
    PublicRequest_Login email pw -> do
      res <- login (signWithKey csk . AuthToken . Identity) email pw
      case res of
        Nothing -> return $ Left $ "Login failed"
        Just token -> return $ Right token
  ApiRequest_Private token r
    | Just (AuthToken (Identity _accId)) <- readSignedWithKey csk token -> case r of
        PrivateRequest_Bar _ -> return $ Right ()
    | otherwise -> error "Unable to authenticate private request"

