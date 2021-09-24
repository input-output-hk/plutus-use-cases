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
import Control.Category

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Rhyolite.Frontend.App

import Common.Api
import Common.Route
import Common.Plutus.Contracts.Uniswap.Types
import Frontend.WebsocketParse

navBar'
  :: forall t m js
  .  ( MonadRhyoliteWidget (DexV (Const SelectedCount)) Api t m
     , Prerender js t m
     , MonadIO (Performable m)
     , SetRoute t (R FrontendRoute) m
     )
  => Maybe Text
  -> m ()
navBar' mWid = do
  e <- navBar mWid
  setRoute $ e

navBar
  :: forall t m js
  .  ( MonadRhyoliteWidget (DexV (Const SelectedCount)) Api t m
     , Prerender js t m
     , MonadIO (Performable m)
     )
  => Maybe Text
  -> m (Event t (R FrontendRoute))
navBar mWid = divClass "navbar navbar-expand-md navbar-dark bg-dark" $ do
  divClass "container-fluid" $ do
    elAttr "a" ("class" =: "navbar-brand" <> "href" =: "#") $ text "POKE-DEX - Plutus Obelisk Koin Economy Decentralized Exchange "
    -- Note: This websocket keeps track of Slot number
    -- ws <- jsonWebSocket ("ws://localhost:8080/ws/" <> wid) (def :: WebSocketConfig t Aeson.Value)
    -- _ <- widgetHold blank $ ffor (_webSocket_recv ws) $ \(a :: Maybe Aeson.Value) -> el "p" $ text $ T.pack $ show aWalletRoute_Portfolio
    case mWid of
      Nothing -> return never
      Just wid -> do
        -- select which page of the dashboard the user would like to see
        navSelect <- elClass "ul" "nav navbar-nav" $ do
          swapEv <- do
            (e,_) <- elAttr' "li" ("class" =: "text-white nav-item ms-5" <> "style" =: "cursor: pointer;") $ text "Swap"
            return $ (WalletRoute_Swap :/ ()) <$ domEvent Click e
          portfolioEv <- do
            (e,_) <- elAttr' "li" ("class" =: "text-white nav-item ms-5" <> "style" =: "cursor: pointer;") $ text "Portfolio"
            return $ (WalletRoute_Portfolio :/ ()) <$ domEvent Click e
          poolEv <- do
            (e,_) <- elAttr' "li" ("class" =: "text-white nav-item ms-5" <> "style" =: "cursor: pointer;") $ text "Pool"
            return $ (WalletRoute_Pool :/ ()) <$ domEvent Click e
          return $ (\r -> FrontendRoute_WalletRoute :/ (wid, r)) <$> leftmost [swapEv, portfolioEv, poolEv]
        -- event that fires once the page has finished loading
        pb <- getPostBuild
        -- recurring event used to poll for wallet balance
        pollingEvent <- tickLossyFromPostBuildTime 10
        requesting_ $ (Api_CallFunds (ContractInstanceId wid)) <$ (leftmost [pb, () <$ pollingEvent])
        elClass "p" "text-white" $ do
          text $ "ADA Balance: "
          fmap (switch . current) $ prerender (return never) $ do
            -- incorporate the use of PAB's websockets to display the wallet's current Ada Balance
            ws <- jsonWebSocket ("ws://localhost:8080/ws/" <> wid) (def :: WebSocketConfig t Aeson.Value)
            -- filter for websocket events relevent to funds that contain the "Funds" tag and "NewObservableState" tag
            let fundsEvent = wsFilterFunds $ _webSocket_recv ws
            widgetHold_ blank $ ffor fundsEvent $ \(mIncomingWebSocketData :: Maybe Aeson.Value) -> case mIncomingWebSocketData of
              Nothing -> return ()
              Just incomingWebSocketData -> do
                -- aeson-lens happened here to unpack the json object received from the websocket
                let currencyDetails = incomingWebSocketData ^. key "contents" . key "Right" . key "contents" . key "getValue" . _Array
                    mAdaBal = fmap fst <$> Map.lookup "" $ parseTokensToMap currencyDetails
                text (T.pack $ show $ fromMaybe 0 mAdaBal)
            return navSelect
