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

module Frontend.Portfolio
  ( portfolioDashboard
  ) where

import Prelude hiding (id, (.), filter)
import Control.Category

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens
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
import Frontend.NavBar
import Frontend.WebsocketParse

portfolioDashboard
  :: forall t m js
  .  ( MonadRhyoliteWidget (DexV (Const SelectedCount)) Api t m
     , Prerender js t m
     , MonadIO (Performable m)
     , SetRoute t (R FrontendRoute) m
     )
  => Text
  -> m ()
portfolioDashboard wid = do
  navBar' $ Just wid
  _ <- divClass "container" $ do
    divClass "pricing-header px-3 py-3 pt-md-5 pb-md-4 mx-auto text-center" $ do
      elClass "h1" "display-5 fw-bold" $ text "Portfolio"
      el "p" $ text "Here are your token balances"
      pb <- getPostBuild
      -- recurring event used to poll for wallet balance
      pollingEvent <- tickLossyFromPostBuildTime 10
      requesting_ $ (Api_CallFunds (ContractInstanceId wid)) <$ (leftmost [pb, () <$ pollingEvent])
      fmap (switch . current) $ prerender (return never) $ do
        -- incorporate the use of PAB's websockets to display the wallet's current Ada Balance
        ws <- jsonWebSocket ("ws://localhost:8080/ws/" <> wid) (def :: WebSocketConfig t Aeson.Value)
        -- filter for websocket events relevent to funds that contain the "Funds" tag and "NewObservableState" tag
        let fundsEvent = wsFilterFunds $ _webSocket_recv ws
        widgetHold_ blank $ ffor fundsEvent $ \(mIncomingFundsWebSocketData :: Maybe Aeson.Value) -> case mIncomingFundsWebSocketData of
          Nothing -> return ()
          Just fundsWebSocketData -> do
            let currencyDetails = fundsWebSocketData ^. key "contents" . key "Right" . key "contents" . key "getValue" . _Array
            elClass "table" "table" $ do
              elClass "thead" "thead-primary" $ el "tr" $ do
                elAttr "th" ("scope" =: "col") $ text "Token Name"
                elAttr "th" ("scope" =: "col") $ text "Balance"
              let formattedTokenDetails = Map.filter
                    -- Note: If token names change, this hash will need to change. Otherwise, the token balances will not be found.
                    (\(_,cs) -> cs == "4195b7e88acc9a061b21f962266770d2e676f76f2a686f6fa15ac155f383c9ad") $
                    parseTokensToMap currencyDetails
              el"tbody" $ do
                forM_ (Map.toList formattedTokenDetails) $ \(tokenName, (tokenBalance,_)) -> do
                  el "tr" $ do
                    el "td" $ text $ T.pack $ show tokenName
                    el "td" $ text $ T.pack $ show tokenBalance
              return ()
            return ()
        return never
  return ()
