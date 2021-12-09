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
  , navBar'
  ) where

import Prelude hiding (id, (.), filter)

import Control.Applicative
import Data.Text (Text)
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Rhyolite.Frontend.App

import Common.Api
import Common.Route

navBar'
  :: forall t m
  .  ( MonadRhyoliteWidget (DexV (Const SelectedCount)) Api t m
     , SetRoute t (R FrontendRoute) m
     )
  => Maybe Text
  -> m ()
navBar' mWid = do
  e <- navBar mWid
  setRoute $ e

navBar
  :: forall t m
  .  ( MonadRhyoliteWidget (DexV (Const SelectedCount)) Api t m
     )
  => Maybe Text
  -> m (Event t (R FrontendRoute))
navBar _ = divClass "navbar navbar-expand-md navbar-dark bg-dark" $ do
  divClass "container-fluid" $ do
    elAttr "a" ("class" =: "navbar-brand" <> "href" =: "#") $ text "POKE-DEX - Plutus Obelisk Koin Economy Decentralized Exchange "
    return never

