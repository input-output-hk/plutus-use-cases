{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}


module Frontend where

import Common.Route
import Control.Monad (void, join)
import Control.Monad.IO.Class
import Data.Either.Combinators (rightToMaybe)
import Data.Functor.Identity
import qualified Data.Text as T
import Frontend.App
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom
import qualified Obelisk.ExecutableConfig as Cfg
import Common.App
import Rhyolite.Account
import Rhyolite.Api
import Rhyolite.Frontend.App
import Rhyolite.Frontend.Cookie
import Rhyolite.Sign

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
    el "title" $ text "User App Template"
  , _frontend_body = do
    configRoute <- liftIO $ Cfg.get "config/common/route"
    let websocketUrl = case configRoute of
          Nothing -> error "Invalid websocket route"
          Just wsroute -> mkWebSocketUri $ wsroute <> (renderBackendRoute checkedRouteEncoder $ BackendRoute_Listen :/ ())
    _ <- prerender blank $ void $ runRhyoliteWidget @DefApp websocketUrl entryPage
    return ()
  }
  where
    mkWebSocketUri address = case T.unpack address of
      'h':'t':'t':'p':'s':uri -> T.pack $ 'w':'s':'s':uri
      'h':'t':'t':'p':uri -> T.pack $ 'w':'s':uri
      'f':'i':'l':'e':uri -> T.pack $ 'w':'s':uri
      _ -> error "Invalid protocol, cannot produce websocket route"

entryPage :: AppWidget t m => m ()
entryPage = do
  -- Check cookies for application authorization token
  doc <- askDocument
  mAuthToken :: Maybe (Signed (AuthToken Identity)) <- do
    cookie <- getCookieJson doc cookieKey
    return $ join $ rightToMaybe <$> cookie
  case mAuthToken of
    Nothing -> do -- When there isn't a authorization token present, show signIn widgets
      email <- inputElement def
      password <- inputElement $ def & initialAttributes .~ ("type" =: "password")
      login <- button "Login"
      let requestPrep = (\e p -> ApiRequest_Public $ PublicRequest_Login e p)
            <$> value email
            <*> value password
          loginReq = tagPromptlyDyn requestPrep login
      loginResp <- requestingIdentity loginReq
      widgetHold_ blank $ ffor loginResp $ \case
        Left _ -> text "Login Failed"
        Right token -> do
          -- Receive token and create cookie with token
          cookie <- defaultCookieJson cookieKey $ Just token
          setPermanentCookie doc cookie
          text "Login Success"
          -- TODO: when login successful, redirect to a login page (use actual route or widgets?)
          -- -- TODO: Make a version with just workflow for quick set ups
          -- -- TODO: Make another version with actual routes for projects with more depth
    Just _token -> return () -- TODO: show authorized application dashboard
