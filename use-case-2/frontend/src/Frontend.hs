{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}

module Frontend where

import Prelude hiding (filter)

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Obelisk.Generated.Static
import Language.Javascript.JSaddle (eval, liftJSM)
import Reflex.Dom.Core
import Rhyolite.Frontend.App

import Common.Api
import Common.Route
import Frontend.ChooseWallet
import Frontend.Pool
import Frontend.Swap
import Frontend.Portfolio

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "link" ("href" =: "https://cdn.jsdelivr.net/npm/bootstrap@5.0.1/dist/css/bootstrap.min.css"
        <> "type" =: "text/css"
        <> "rel" =: "stylesheet"
        <> "integrity" =: "sha384-+0n0xVW2eSR5OomGNYDnhzAbDsOXxcvSN1TPprVMTNDbiYZCxYbOOl7+AMvyTG2x"
        <> "crossorigin" =: "anonymous"
        ) blank
  , _frontend_body = do
      -- `prerender` and `prerender_` let you choose a widget to run on the server
      -- during prerendering and a different widget to run on the client with
      -- JavaScript. The following will generate a `blank` widget on the server and
      -- print "Hello, World!" on the client.
      prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: T.Text)
      let errorLeft e = case e of
            Left _ -> error "runFrontend: Unexpected non-app ObeliskRoute reached the frontend. This shouldn't happen."
            Right x -> x
      let validFullEncoder = errorLeft $ checkEncoder fullRouteEncoder
      _ <- runObeliskRhyoliteWidget vesselToWire "common/route" validFullEncoder (BackendRoute_Listen :/ ()) app
      return ()
  }

app
  :: forall t m js
  .  ( MonadRhyoliteWidget (DexV (Const SelectedCount)) Api t m
     , Prerender js t m
     , MonadIO (Performable m)
     , SetRoute t (R FrontendRoute) m
     )
  => RoutedT t (R FrontendRoute) m ()
app = subRoute_ $ \case
  FrontendRoute_ChooseWallet -> chooseWallet
  FrontendRoute_WalletRoute -> do
    dAddr <- fmap fst <$> askRoute
    withRoutedT (fmap $ view _2) $ subRoute_ $ \case
      WalletRoute_Swap -> dyn_ $ swapDashboard <$> dAddr
      WalletRoute_Portfolio -> dyn_ $ portfolioDashboard <$> dAddr
      WalletRoute_Pool -> dyn_ $ poolDashboard <$> dAddr
