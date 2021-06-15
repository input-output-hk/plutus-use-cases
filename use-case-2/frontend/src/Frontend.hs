{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}

module Frontend where

import Prelude hiding (id, (.), filter)
import Control.Category

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens
import Data.Int
import qualified Data.Map as Map
import Data.Scientific (coefficient)
import Data.Semigroup (First(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
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
import Safe (headMay)

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

app :: forall t m js. (MonadRhyoliteWidget (DexV (Const SelectedCount)) Api t m, Prerender js t m, MonadIO (Performable m)) => Workflow t m ()
app = Workflow $ do
  _ <- navBar Nothing
  divClass "p-5 mb-4 bg-light rounded-5" $ do
    divClass "container-fluid py-5" $ do
      elClass "h3" "display-5 fw-bold" $ text "Wallet Accounts"
      elClass "p" "lead" $ text "Choose one of the avaiable wallets below: "
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
                -- TODO: Add some highlight on hover for list items
                (e,_) <- elAttr' "li" ("class" =: "list-group-item list-group-item-dark" <> "style" =: "cursor:pointer") $ text wid
                return $ wid <$ domEvent Click e
            return $ leftmost walletIdEvents
      return ((), dashboard <$> walletEv)

navBar :: forall t m js. (MonadRhyoliteWidget (DexV (Const SelectedCount)) Api t m, Prerender js t m, MonadIO (Performable m)) => Maybe Text -> m (Event t ())
navBar mWid = divClass "navbar navbar-expand-md navbar-dark bg-dark" $ do
  divClass "container-fluid" $ do
    elAttr "a" ("class" =: "navbar-brand" <> "href" =: "#") $ text "POKE-DEX - Plutus Obelisk Koin Economy Decentralized Exchange "
      -- Note: This websocket keeps track of Slot number
      --   el "p" $ text "-------------------------------"
      -- Note: This websocket keeps track of Slot number
      -- ws <- jsonWebSocket ("ws://localhost:8080/ws/" <> wid) (def :: WebSocketConfig t Aeson.Value)
      -- _ <- widgetHold blank $ ffor (_webSocket_recv ws) $ \(a :: Maybe Aeson.Value) -> el "p" $ text $ T.pack $ show a
    case mWid of
      Nothing -> return never
      Just wid -> do
        pollingEvent <- tickLossyFromPostBuildTime 10
        -- pollingEvent <- button "See Funds"
        requesting_ $ (Api_CallFunds (ContractInstanceId wid)) <$ pollingEvent
        fmap (switch . current) $ prerender (return never) $ do
          -- incorporate the use of PAB's websockets to display the wallet's current Ada Balance
          ws <- jsonWebSocket ("ws://localhost:8080/ws/" <> wid) (def :: WebSocketConfig t Aeson.Value)
          -- filter for websocket events relevent to funds that contain the "Funds" tag and "NewObservableState" tag
          let fundsEvent = flip ffilter  (_webSocket_recv ws) $ \(mIncomingWebSocketData :: Maybe Aeson.Value) -> case mIncomingWebSocketData of
                Nothing -> False
                Just incomingWebSocketData -> do
                  let observableStateTag = incomingWebSocketData ^.. key "tag" . _String
                      fundsTag = incomingWebSocketData ^.. key "contents" . key "Right" . key "tag" . _String
                  observableStateTag == ["NewObservableState"] && fundsTag == ["Funds"]
          _ <- widgetHold blank $ ffor fundsEvent $ \(mIncomingWebSocketData :: Maybe Aeson.Value) -> case mIncomingWebSocketData of
            Nothing -> return ()
            Just incomingWebSocketData -> do
              -- aeson-lens happened here to unpack the json object received from the websocket
              -- TODO: Place aeson lens selectors into it's own module
              let currencyDetails = incomingWebSocketData ^.. key "contents" . key "Right" . key "contents" . key "getValue" . _Array -- DEBUG
                  adaDetails = (V.! 1) <$> currencyDetails
                  adaNameWithBalance = adaDetails ^.. traverse . _Array
                  nestedArrayIndex1 = (V.! 1) <$> adaNameWithBalance
                  unwrapArray1 = nestedArrayIndex1 ^.. traverse . _Array
                  nestedArrayIndex2 = (V.! 0) <$> unwrapArray1
                  unwrapArray2 = nestedArrayIndex2 ^.. traverse . _Array
                  nestedArrayIndex3 = (V.! 1) <$> unwrapArray2
                  balanceList = nestedArrayIndex3 ^.. traverse . _Number
                  mAdaBalance = headMay balanceList
                  adaBalance = case mAdaBalance of
                    Nothing -> 0
                    Just bal -> coefficient bal
              elClass "p" "text-white" $ text $ "ADA Balance: " <> (T.pack $ show adaBalance)
          return never

dashboard :: forall t m js. (MonadRhyoliteWidget (DexV (Const SelectedCount)) Api t m, Prerender js t m, MonadIO (Performable m)) => Text -> Workflow t m ()
dashboard wid = Workflow $ do
  -- TODO: Add swap and stake tabs to the navbar
  _ <- navBar $ Just wid
  _ <- divClass "p-5 mb-4 bg-light rounded-5" $ do
    divClass "container-fluid py-5" $ do
      elClass "h3" "display-5 fw-bold" $ text "Swap Tokens"
      el "p" $ text "What would you like to swap?"
      dmmPooledTokens <- viewPooledTokens
      _ <- switchHold never <=< dyn $ ffor dmmPooledTokens $ \case
        Nothing -> return never
        Just mPoolTokens -> case mPoolTokens of
          Nothing -> return never
          Just poolTokens -> do
            let dropdownList = Map.fromListWith (\_ tkName -> tkName) $ flip fmap poolTokens $ \pt ->
                  if _pooledToken_name pt == "" then (pt, "ADA") else (pt, _pooledToken_name pt)
                firstOption = headMay $ Map.keys dropdownList
            case firstOption of
              Nothing -> do
                elClass "p" "text-warning" $ text "There are no tokens available to swap."
                return never
              Just fstOpt -> do
                divClass "form container" $ do
                  divClass "form-group" $ do
                    -- Select first token and amount
                    -- TODO: Create a convenient widget function out of the dropdown text input for coins
                    (selectionA, amountA) <- divClass "input-group row" $ do
                      coinAChoice <- dropdown fstOpt (constDyn $ dropdownList) $
                        def { _dropdownConfig_attributes = constDyn ("class" =: "form-control col-md-1") }
                      coinAAmountInput <- inputElement $ def
                        & inputElementConfig_elementConfig . elementConfig_initialAttributes
                          .~ ("class" =: "form-control col-md-4" <> "type" =: "number")
                        & inputElementConfig_initialValue
                          .~ ("0" :: Text)
                      return (_dropdown_value coinAChoice, _inputElement_value coinAAmountInput)
                    -- Select second token and amount
                    (selectionB, amountB) <- divClass "input-group row mt-3" $ do
                      coinBChoice <- dropdown fstOpt (constDyn $ dropdownList) $
                        def { _dropdownConfig_attributes = constDyn ("class" =: "form-control col-md-1") }
                      coinBAmountInput <- inputElement $ def
                        & inputElementConfig_elementConfig . elementConfig_initialAttributes
                          .~ ("class" =: "form-control col-md-4" <> "type" =: "number")
                        & inputElementConfig_initialValue
                          .~ ("0" :: Text)
                      return (_dropdown_value coinBChoice, _inputElement_value coinBAmountInput)
                    swap <- divClass "input-group row mt-3" $ do
                      (e,_) <- elClass' "button" "btn btn-primary" $ text "Swap"
                      return $ domEvent Click e
                    let pooledTokenToCoin pt = Coin $ AssetClass (CurrencySymbol (_pooledToken_symbol pt), TokenName (_pooledToken_name pt))
                        toAmount amt = Amount $ (read (T.unpack amt) :: Integer) -- TODO: is read ok?
                        requestLoad = ((\w c1 c2 a1 a2 -> Api_Swap w c1 c2 a1 a2)
                          <$> (constDyn $ ContractInstanceId wid)
                          <*> (pooledTokenToCoin <$> selectionA)
                          <*> (pooledTokenToCoin <$> selectionB)
                          <*> (toAmount <$> amountA)
                          <*> (toAmount <$> amountB))
                    swapResponse <- requesting $ tagPromptlyDyn requestLoad swap
                    -- TODO: "Success needs to differentiate between being submitted to chain and the swap actually occurring successfully.
                    widgetHold_ blank $ ffor swapResponse $ \ieResponse -> case runIdentity ieResponse of
                      Left err -> elClass "p" "text-danger" $ text $ T.pack $ show err
                      Right _ -> elClass "p" "text-success" $ text "Success!"
                    return ()
                return never
      return ()
  return ((), never)

viewCounter :: (MonadQuery t (Vessel Q (Const SelectedCount)) m, Reflex t) => m (Dynamic t (Maybe (Maybe Int32)))
viewCounter = (fmap.fmap.fmap) (getFirst . runIdentity) $ queryViewMorphism 1 $ constDyn $ vessel Q_Counter . identityV

viewContracts :: (MonadQuery t (Vessel Q (Const SelectedCount)) m, Reflex t) => m (Dynamic t (Maybe (Maybe [Text])))
viewContracts = (fmap.fmap.fmap) (getFirst . runIdentity) $ queryViewMorphism 1 $ constDyn $ vessel Q_ContractList . identityV

viewPooledTokens :: (MonadQuery t (Vessel Q (Const SelectedCount)) m, Reflex t) => m (Dynamic t (Maybe (Maybe [PooledToken])))
viewPooledTokens = (fmap.fmap.fmap) (getFirst . runIdentity) $ queryViewMorphism 1 $ constDyn $ vessel Q_PooledTokens . identityV
