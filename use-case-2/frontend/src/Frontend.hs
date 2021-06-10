{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}

module Frontend where

import Prelude hiding (id, (.), filter)
import Control.Category

import Control.Monad
import qualified Data.Aeson as Aeson
import Data.Int
import Data.Semigroup (First(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Vessel
import Data.Vessel.Identity
import Data.Vessel.Vessel
import Data.Vessel.ViewMorphism
import Language.Javascript.JSaddle (eval, liftJSM, JSM)
-- import Obelisk.Configs
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Generated.Static
import Reflex.Dom.Core
-- import Reflex.Dom.WebSocket
import Rhyolite.Frontend.App

import Common.Api
import Common.Route
import Common.Plutus.Contracts.Uniswap.Types
import Common.Schema


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
      _ <- runObeliskRhyoliteWidget vesselToWire "common/route" validFullEncoder (BackendRoute_Listen :/ ()) $ workflow $ app
      return ()
  }

app :: forall t m js. (MonadRhyoliteWidget (DexV (Const SelectedCount)) Api t m, Prerender js t m) => Workflow t m ()
app = Workflow $ do
  _ <- navBar Nothing
  divClass "p-5 mb-4 bg-light rounded-5" $ do
    divClass "container-fluid py-5" $ do
      elClass "h3" "display-5 fw-bold" $ text "Wallet Accounts"
      elClass "p" "lead" $ text "Choose one of the avaiable wallets below: "
      -- TODO: Make a way to select one of these wallets
      dmmWalletIds <- viewContracts
      walletEv <- switchHold never <=< dyn $ ffor dmmWalletIds $ \case
        Nothing -> do
          text "There are no wallets avaiable"
          return never
        Just mWalletIds -> case mWalletIds of
          Nothing -> do
            text "There are no wallets avaiable"
            return never
          Just walletIds -> do
            walletIdEvents <- elClass "ul" "list-group" $ do
              forM walletIds $ \wid -> fmap (switch . current) $ prerender (return never) $ do
                -- Note: This websocket keeps track of Slot number
                --   el "p" $ text "-------------------------------"
                -- Note: This websocket keeps track of Slot number
                -- ws <- jsonWebSocket ("ws://localhost:8080/ws/" <> wid) (def :: WebSocketConfig t Aeson.Value)
                -- _ <- widgetHold blank $ ffor (_webSocket_recv ws) $ \(a :: Maybe Aeson.Value) -> el "p" $ text $ T.pack $ show a

                -- TODO: Add some highlight on hover for list items
                (e,_) <- elAttr' "li" ("class" =: "list-group-item list-group-item-dark" <> "style" =: "cursor:pointer") $ text wid
                return $ wid <$ domEvent Click e
            return $ leftmost walletIdEvents
      return ((), dashboard <$> walletEv)

navBar :: forall t m js. (Prerender js t m, DomBuilder t m) => Maybe Text -> m (Event t ())
navBar mWid = divClass "navbar navbar-expand-md navbar-dark bg-dark" $ do
  divClass "container-fluid" $ do
    elAttr "a" ("class" =: "navbar-brand" <> "href" =: "#") $ text "POKE-DEX - Plutus Obelisk Koin Economy Decentralized Exchange "
    -- TODO: incorporate the use of PAB's websockets to display the wallet's current Ada Balance
    case mWid of
      Nothing -> return never
      Just wid -> fmap (switch . current) $ prerender (return never) $ do
        ws<- jsonWebSocket ("ws://localhost:8080/ws/" <> wid) (def :: WebSocketConfig t Aeson.Value)
        _ <- widgetHold blank $ ffor (_webSocket_recv ws) $ \(a :: Maybe Aeson.Value) -> do
          elClass "p" "text-white" $ text $ T.pack $ show a
        return never

dashboard :: forall t m js. (MonadRhyoliteWidget (DexV (Const SelectedCount)) Api t m, Prerender js t m) => Text -> Workflow t m ()
dashboard wid = Workflow $ do
  _ <- navBar $ Just wid
  -- TODO: Get a list of coins that are supported in the token pool
  divClass "p-5 mb-4 bg-light rounded-5" $ do
    divClass "container-fluid py-5" $ do
      elClass "h3" "display-5 fw-bold" $ text "List of Swappable Tokens"
      el "p" $ text "Here is the list of tokens we support: "
      display =<< viewPooledTokens
      return ()
  -- Example of making a swap call
  el "div" $ do
  divClass "p-5 mb-4 bg-light rounded-5" $ do
    divClass "container-fluid py-5" $ do
      elClass "h3" "display-5 fw-bold" $ text "Swap"
      swap <- button "swap"
      -- TODO: Values passed to swap should not be hard coded.
      requesting_ $ Api_Swap
        (ContractInstanceId "0eb3011f-40e7-4d38-a4af-7602df8c3bb3")
        (Coin $ AssetClass (CurrencySymbol "", TokenName ""))
        (Coin (AssetClass (CurrencySymbol "7c7d03e6ac521856b75b00f96d3b91de57a82a82f2ef9e544048b13c3583487e", TokenName "A")))
        (Amount 112)
        (Amount 0) <$ swap
  return ((), never)

viewCounter :: (MonadQuery t (Vessel Q (Const SelectedCount)) m, Reflex t) => m (Dynamic t (Maybe (Maybe Int32)))
viewCounter = (fmap.fmap.fmap) (getFirst . runIdentity) $ queryViewMorphism 1 $ constDyn $ vessel Q_Counter . identityV

viewContracts :: (MonadQuery t (Vessel Q (Const SelectedCount)) m, Reflex t) => m (Dynamic t (Maybe (Maybe [Text])))
viewContracts = (fmap.fmap.fmap) (getFirst . runIdentity) $ queryViewMorphism 1 $ constDyn $ vessel Q_ContractList . identityV

viewPooledTokens :: (MonadQuery t (Vessel Q (Const SelectedCount)) m, Reflex t) => m (Dynamic t (Maybe (Maybe [PooledToken])))
viewPooledTokens = (fmap.fmap.fmap) (getFirst . runIdentity) $ queryViewMorphism 1 $ constDyn $ vessel Q_PooledTokens . identityV
