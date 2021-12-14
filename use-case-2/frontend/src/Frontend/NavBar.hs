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

module Frontend.NavBar
  ( navBar
 --  , navBar'
  ) where

import Prelude hiding (id, (.), filter)

import Control.Applicative
import Control.Monad ((<=<), void, forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Text (Text)
import Language.Javascript.JSaddle
import Obelisk.Route
import Reflex.Dom.Core
import Rhyolite.Frontend.App

import Common.Api
import Common.Route

navBar
  :: forall t m js
  .  ( MonadRhyoliteWidget (DexV (Const SelectedCount)) Api t m
     , Prerender js t m
     )
  => Dynamic t Text
  -> m (Event t Text)
navBar dynAddr = divClass "navbar navbar-expand-md navbar-white bg-white" $ divClass "container-fluid" $ do
  -- determine whether to show button or address
  connectNamiEv <- switchHold never <=< dyn $ ffor dynAddr $ \case
    "" -> do
      el "p" $ text ""
      (e,_) <- elClass' "button" "btn btn-outline-dark" $ text "Connect Nami Wallet"
      return $ domEvent Click e
    addr -> do
      el "p" $ text ""
      el "p" $ text $ (T.take 14 addr) <> "..." <> T.takeEnd 5 addr
      return never
  -- Allow user to connect to nami wallet
  (enableNamiEv, enableNamiTrigger) <- newTriggerEvent
  prerender_ blank $ performEvent_ $ (liftJSM $ void $ do
    let enabledNamiCallback :: JSCallAsFunction
        enabledNamiCallback = fun $ \_ _ args -> case args of
          (i:_) -> do
            fromJSVal i >>= \case
              Just (a :: Text) -> liftIO (enableNamiTrigger a)
              _ -> pure ()
          _ -> pure ()
    -- enableNami <- eval ("await window.cardano.enable();" :: Text)
    enableNami <- eval ("(async function foo (someparam) { let x = await await window.cardano.enable(); \
            \ console.log(x); \
            \ someparam(x); })" :: Text)
    _ <- call enableNami enableNami (enabledNamiCallback)
    return ()) <$ connectNamiEv
  return enableNamiEv

