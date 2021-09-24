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
      -- prerender separates the code which can be run on the backend (to
      -- prerender), from the code that is run on the frontend. We don't use the
      -- browser way of listening to the websocket on the backend (though in
      -- principle we could listen to the websocket a different way), and so we
      -- return a "never firing" event on the backend to both branches return
      -- the same type.
      --
      -- the prerender will splice together the server-rendered and
      -- client-animated versions, wrapping the result value in a "dynamic", so
      -- we have "dynamic of an event of a maybe", hence the DEM in the name of
      -- the variable.
      respDEM <- prerender (return never) $ do
        -- incorporate the use of PAB's websockets to display the wallet's current Ada Balance
        ws <- jsonWebSocket ("ws://localhost:8080/ws/" <> wid) (def :: WebSocketConfig t Aeson.Value)
        -- filter for websocket events relevent to funds that contain the "Funds" tag and "NewObservableState" tag
        pure $ wsFilterFunds $ _webSocket_recv ws
      -- Squash together the dynamic and event into a single dynamic, but with
      -- an extra maybe. The outer maybe is 'Nothing' when we have yet to hear a
      -- response, and 'Just' once we have a result. That means once the dynamic
      -- becomes an outer 'Just', it should never go back to being an outer
      -- 'Nothing'.
      respDMM <- holdDyn Nothing $ fmap Just $ switch $ current respDEM
      -- "factor" the dynamic of maybe so that the innerside has it's own
      -- dynamic, and when we change from one "Just ..." to another "Just ...",
      -- only the inner dynamic changes. This doesn't make much of difference
      -- here, but is good practice in general to minimize DOM changes. (You can compare React's virtual DOM, but we are deduping on the *input* rather than *output* side, which is more efficient.)
      respDMDM <- maybeDyn respDMM
      dyn_ $ ffor respDMDM $ \case
        Nothing -> text "Waiting for response from PAB..."
        Just respDM -> dyn_ $ ffor respDM $ \case
          Nothing -> text "error: missing key in JSON from PAB."
          Just fundsWebSocketData -> do
            let currencyDetails = fundsWebSocketData ^. key "contents" . key "Right" . key "contents" . key "getValue" . _Array
            let formattedTokenDetails = Map.filter
                  -- Note: If token names change, this hash will need to change. Otherwise, the token balances will not be found.
                  (\(_,cs) -> cs == "4195b7e88acc9a061b21f962266770d2e676f76f2a686f6fa15ac155f383c9ad") $
                  parseTokensToMap currencyDetails
            if Map.null formattedTokenDetails
              then
                text "PAB reports empty portfolio, this probably means it is still loading, rather than there actually being an empty portfolio."
              else do
                elClass "table" "table" $ do
                  elClass "thead" "thead-primary" $ el "tr" $ do
                    elAttr "th" ("scope" =: "col") $ text "Token Name"
                    elAttr "th" ("scope" =: "col") $ text "Balance"
                  el "tbody" $ do
                    forM_ (Map.toList formattedTokenDetails) $ \(tokenName, (tokenBalance,_)) -> do
                      el "tr" $ do
                        el "td" $ text $ T.pack $ show tokenName
                        el "td" $ text $ T.pack $ show tokenBalance
                  return ()
                return ()
  return ()
