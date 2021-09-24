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

module Frontend.Swap
  ( swapDashboard
  ) where

import Prelude hiding (id, (.), filter)
import Control.Category

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.Semigroup (First(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Vessel
import Data.Vessel.Identity
import Data.Vessel.Vessel
import Data.Vessel.ViewMorphism
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Rhyolite.Frontend.App
import Safe (headMay, initMay, readMay)

import Common.Api
import Common.Route
import Common.Plutus.Contracts.Uniswap.Types
import Common.Plutus.Contracts.Uniswap.Estimates
import Common.Schema
import Frontend.NavBar
import Frontend.WebsocketParse
import Frontend.Widgets

swapDashboard
  :: forall t m js
  .  ( MonadRhyoliteWidget (DexV (Const SelectedCount)) Api t m
     , Prerender js t m
     , MonadIO (Performable m)
     , SetRoute t (R FrontendRoute) m
     )
  => Text
  -> m ()
swapDashboard wid = do
  navBar' $ Just wid
  pollRefreshPoolBalance wid
  pabEV :: Event t (Maybe Aeson.Value) <- fmap (switch . current) $ prerender (return never) $ do
    _webSocket_recv <$> jsonWebSocket ("ws://localhost:8080/ws/" <> wid) (def :: WebSocketConfig t Aeson.Value)
  divClass "container" $ do
    divClass "pricing-header px-3 py-3 pt-md-5 pb-md-4 mx-auto text-center" $ do
      elClass "h1" "display-5 fw-bold" $ text "Swap Tokens"
      el "p" $ text "What would you like to swap?"
    -- widget that contains the swap form
    divClass "card-group mb-3 text-center" $ do
      formEvent <- selectCoins wid pabEV
      transactionDetails wid pabEV formEvent

selectCoins
  :: forall t m js
  .  ( MonadRhyoliteWidget (DexV (Const SelectedCount)) Api t m
     , Prerender js t m
     , MonadIO (Performable m)
     , SetRoute t (R FrontendRoute) m
     )
  => Text
  -> Event t (Maybe Aeson.Value)
  -> m (Event t (Maybe ((PooledToken, Text), (PooledToken, Text))))
selectCoins wid pabEV = do
      dmmPooledTokens <- viewPooledTokens
      switchHold never <=< dyn $ ffor dmmPooledTokens $ \case
        Nothing -> return never
        Just mPoolTokens -> case mPoolTokens of
          Nothing -> return never
          Just poolTokens -> do
            let dropdownList = Map.fromListWith (\_ tkName -> tkName) $ flip fmap poolTokens $ \pt ->
                  if _pooledToken_name pt == "" then (pt, "ADA") else (pt, _pooledToken_name pt)
                twoOptions = initMay $ Map.keys dropdownList
            case twoOptions of
              Just (fstOpt:sndOpt:_) -> do
                divClass "col" $ divClass "card mb-4 box-shadow h-100 mx-3" $ do
                  divClass "card-header" $ elClass "h4" "my-0 font-weight-normal" $ text "Select Coins"
                  divClass "card-body" $ divClass "form container" $ divClass "form-group" $ mdo
                    -- Select first token and amount
                    (selectionA, amountA) <- divClass "input-group row" $ do
                      coinAChoice <- dropdown fstOpt (constDyn $ dropdownList) $
                        def { _dropdownConfig_attributes = constDyn ("class" =: "form-control") }
                      coinAAmountInput <- inputElement $ def
                        & inputElementConfig_elementConfig . elementConfig_initialAttributes
                          .~ ("class" =: "form-control" <> "type" =: "number")
                        & inputElementConfig_initialValue
                          .~ ("0" :: Text)
                      return (_dropdown_value coinAChoice, _inputElement_value coinAAmountInput)
                    let noduplicateDropdownList = Map.filterWithKey (\k _ -> k /= fstOpt) dropdownList
                    dynNonDuplicateDropdownList <- holdDyn noduplicateDropdownList $ ffor (updated selectionA)
                      $ \choice -> Map.filterWithKey (\k _ -> k /= choice) dropdownList
                    -- Select second token and amount
                    (selectionB, amountB) <- divClass "input-group row mt-3" $ do
                      coinBChoice <- dropdown sndOpt dynNonDuplicateDropdownList $ DropdownConfig
                        { _dropdownConfig_attributes = constDyn ("class" =: "form-control")
                        , _dropdownConfig_setValue = (ffor (updated dynNonDuplicateDropdownList) $ \opts -> fst $ Map.elemAt 0 opts)
                        }
                      coinBAmountInput <- inputElement $ def
                        & inputElementConfig_elementConfig . elementConfig_initialAttributes
                          .~ ("class" =: "form-control" <> "type" =: "number")
                        & inputElementConfig_initialValue
                          .~ ("0" :: Text)
                      return (_dropdown_value coinBChoice, _inputElement_value coinBAmountInput)
                    btnEnabled <- toggle True $ leftmost [swap, () <$ observableStateEv]
                    swap <- divClass "input-group row mt-3" $ do
                      (e,_) <- elDynAttr' "button" (toggleBtnUsability <$> btnEnabled) $ do
                        elDynAttr "span" (toggleSpinner <$> btnEnabled) blank
                        widgetHold_ (text "Swap") $ ffor (updated btnEnabled) $ \case
                          True -> text "Swap"
                          False -> text "Loading..."
                      return $ domEvent Click e
                    let ffor4 a b c d f = liftA3 f a b c <*> d
                        pooledTokenToCoin pt = Coin $ AssetClass (CurrencySymbol (_pooledToken_symbol pt), TokenName (_pooledToken_name pt))
                        toAmount amt = Amount $ (read (T.unpack amt) :: Integer)
                        requestLoad = ((\w c1 c2 a1 a2 -> Api_Swap w c1 c2 a1 a2)
                          <$> (constDyn $ ContractInstanceId wid)
                          <*> (pooledTokenToCoin <$> selectionA)
                          <*> (pooledTokenToCoin <$> selectionB)
                          <*> (toAmount <$> amountA)
                          <*> (toAmount <$> amountB))
                    -- This response returns transaction fee information, it does not return the swap response
                    -- TODO still, do *something* with it, to handle failure cases?
                    _responseVal  <- requesting $ tagPromptlyDyn requestLoad swap
                    observableStateEv <- do
                      let
                          observableStateEvent :: Event t Aeson.Value
                          observableStateEvent = flip fmapMaybe pabEV $ \(mIncomingWebSocketData :: Maybe Aeson.Value )
                            -> case mIncomingWebSocketData of
                              Nothing -> Nothing
                              Just incomingWebSocketData -> do
                                let newObservableStateTag = incomingWebSocketData ^.. key "tag" . _String
                                guard $ newObservableStateTag == ["NewObservableState"]
                                incomingWebSocketData ^? key "contents"
                          observableStateSuccessEvent = flip ffilter observableStateEvent $ \(incomingWebSocketData :: Aeson.Value )
                            -> do
                              let swappedTag = incomingWebSocketData ^.. key "Right" . key "tag" . _String
                              swappedTag == ["Swapped"]
                          observableStateFailureEvent = flip ffilter observableStateEvent $ \(incomingWebSocketData :: Aeson.Value )
                            -> do
                              let failureMessageTag = incomingWebSocketData ^.. key "Left" . _String
                              failureMessageTag /= []
                      -- this event will cause the success message to disappear when it occurs
                      vanishEvent <- delay 7 observableStateSuccessEvent
                      -- show success message based on new observable state
                      widgetHold_ blank $ ffor (leftmost [Just <$> observableStateSuccessEvent, Nothing <$ vanishEvent]) $
                        \(mIncomingWebSocketData :: Maybe Aeson.Value) -> case mIncomingWebSocketData of
                          Nothing -> blank
                          Just _ -> elClass "p" "text-success" $ text "Success!"
                      widgetHold_ blank $ ffor observableStateFailureEvent $
                        \(incomingWebSocketData :: Aeson.Value) -> do
                            let errMsg = incomingWebSocketData ^.. key "Left" . _String
                            elClass "p" "text-danger" $ text $ T.concat errMsg
                      return $ leftmost [observableStateFailureEvent, observableStateSuccessEvent]
                    return $ updated $ fmap Just $ ffor4 selectionA amountA selectionB amountB $ \selA amtA selB amtB -> ((selA, amtA), (selB, amtB))
              _ -> do
                elClass "p" "text-warning" $ text "There are no tokens available to swap."
                return never

transactionDetails
  :: forall t m js
  .  ( MonadRhyoliteWidget (DexV (Const SelectedCount)) Api t m
     , Prerender js t m
     , MonadIO (Performable m)
     , SetRoute t (R FrontendRoute) m
     )
  => Text
  -> Event t (Maybe Aeson.Value)
  -> Event t (Maybe ((PooledToken, Text), (PooledToken, Text)))
  -> m ()
transactionDetails wid pabEV formEvent = do
      -- widget that shows transaction details such as swap estimates, etc.
      divClass "col" $ divClass "card mb-4 box-shadow h-100" $ do
        divClass "card-header" $ elClass "h4" "my-0 font-weight-normal" $ text "Transaction Details"
        divClass "card-body" $ do
          poolMapEv <- do
            let poolsEvent = wsFilterPools $ pabEV
            dynPoolMap <- holdDyn Map.empty $ ffor poolsEvent $ \mIncomingPoolsWebSocketData -> do
              let poolDetails = case mIncomingPoolsWebSocketData of
                    Nothing -> V.empty
                    Just poolsWebSocketData -> poolsWebSocketData ^. key "contents" . key "Right" . key "contents" . _Array
              parseLiquidityTokensToMap poolDetails
            return $ fmap Just $ updated dynPoolMap
          -- combine events from from updates and smart contract pool data to perform swap estimates
          dynPoolMap :: Dynamic t (Maybe (Map.Map Text (Integer, ((Text, Integer), (Text, Integer)))))
            <- holdDyn Nothing poolMapEv
          poolAndFormEvent <- holdDyn (Nothing, Nothing) $ attachPromptlyDyn dynPoolMap formEvent
          let swapEstimate = ffor poolAndFormEvent $ \case
                (Just poolMap, Just ((selA, amtA), (selB, amtB))) -> do
                  let ((coinAName, coinAPoolAmount), (coinBName, coinBPoolAmount)) = fromMaybe (("", 0), ("", 0))
                        $ fmap snd
                        $ headMay
                        $ Map.elems
                        $ Map.filter (\(_,((tknameA,_), (tknameB, _)))
                        -> tknameA == (_pooledToken_name selA) && tknameB == (_pooledToken_name selB)) poolMap
                  let amtA' :: Integer = fromMaybe 0 $ readMay $ T.unpack amtA
                      amtB' :: Integer = fromMaybe 0 $ readMay $ T.unpack amtB
                      (swapAmount, estimatedTkName) = if amtA' == 0 then (amtB',coinAName) else (amtA',coinBName)
                  (findSwapA coinAPoolAmount coinBPoolAmount swapAmount, estimatedTkName)
                _ -> (0, "")
          txFeeEstimateResp <- requesting $ fmap (\sca -> Api_EstimateTransactionFee sca) $ SmartContractAction_Swap <$ formEvent
          widgetHold_ blank $ ffor (updated swapEstimate) $ \(estimate, eTokenName) ->
            elClass "p" "text-info" $ text
              $ "Estimated to receive "
              <> (T.pack $ show estimate)
              <> " " <> (T.pack $ show $ if "" == eTokenName then "ADA" else eTokenName)
          widgetHold_ blank $ ffor txFeeEstimateResp $ \txFeeEstimate ->
            elClass "p" "text-warning" $ text
              $ "Estimated transaction fee: "
              <> (T.pack $ show $ runIdentity txFeeEstimate)
              <> " ADA"

viewPooledTokens
  :: ( MonadQuery t (Vessel Q (Const SelectedCount)) m
     , Reflex t
     )
  => m (Dynamic t (Maybe (Maybe [PooledToken])))
viewPooledTokens = (fmap.fmap.fmap) (getFirst . runIdentity) $ queryViewMorphism 1 $ constDyn $ vessel Q_PooledTokens . identityV

-- | Nothing is returned, because this is just a command to tell the backend to
-- poll the PAB for new data to *push* to our live queries.
pollRefreshPoolBalance
  :: forall t m
  .  ( MonadRhyoliteWidget (DexV (Const SelectedCount)) Api t m
     , MonadIO (Performable m)
     )
  => Text
  -> m ()
pollRefreshPoolBalance wid = do
  pb <- getPostBuild
  -- recurring event used to poll for pool balance
  pollingEvent <- tickLossyFromPostBuildTime 10
  requesting_ $ (Api_CallPools (ContractInstanceId wid)) <$ (leftmost [pb, () <$ pollingEvent])
