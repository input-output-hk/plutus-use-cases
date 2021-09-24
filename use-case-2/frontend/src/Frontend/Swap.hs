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
import Control.Monad.Fix
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
  pabDEV :: Dynamic t (Event t (Maybe Aeson.Value)) <- prerender (return never) $
    _webSocket_recv <$> jsonWebSocket ("ws://localhost:8080/ws/" <> wid) (def :: WebSocketConfig t Aeson.Value)
  pabDMMV :: Dynamic t (Maybe (Maybe Aeson.Value)) <- holdDyn Nothing $ fmap Just $ switch $ current pabDEV
  divClass "container" $ do
    divClass "pricing-header px-3 py-3 pt-md-5 pb-md-4 mx-auto text-center" $ do
      elClass "h1" "display-5 fw-bold" $ text "Swap Tokens"
      el "p" $ text "What would you like to swap?"
    -- widget that contains the swap form
    divClass "card-group mb-3 text-center" $ do
      formDyn <- selectCoins wid
      transactionDetails pabDMMV formDyn

selectCoins
  :: forall t m
  .  ( MonadRhyoliteWidget (DexV (Const SelectedCount)) Api t m
     , MonadIO (Performable m)
     )
  => Text
  -> m (Dynamic t (Maybe ((PooledToken, Text), (PooledToken, Text))))
selectCoins wid = do
  divClass "col" $ divClass "card mb-4 box-shadow h-100 mx-3" $ do
    divClass "card-header" $ elClass "h4" "my-0 font-weight-normal" $ text "Select Coins"
    divClass "card-body" $ divClass "form container" $ divClass "form-group" $ do
      dmmPooledTokens <- viewPooledTokens
      let asdf :: Event t (Dynamic t (Maybe a))
               -> m (Dynamic t (Maybe a))
          asdf = fmap join . holdDyn (pure Nothing) -- TODO too prompt
      (=<<) asdf $ dyn $ ffor dmmPooledTokens $ \case
        Nothing -> do
          text "loading pool tokens..."
          return $ pure Nothing
        Just mPoolTokens -> case mPoolTokens of
          Nothing -> do
            text "no pool tokens"
            return $ pure Nothing
          Just poolTokens -> do
            let dropdownList = Map.fromListWith (\_ tkName -> tkName) $ flip fmap poolTokens $ \pt ->
                  if _pooledToken_name pt == "" then (pt, "ADA") else (pt, _pooledToken_name pt)
                twoOptions = initMay $ Map.keys dropdownList
            case twoOptions of
              Just (fstOpt:sndOpt:_) -> mdo
                    -- Select first token and amount
                    (selectionA, amountA) <- divClass "input-group row" $ do
                      coinAChoice <- dropdown fstOpt (constDyn $ dropdownList) $
                        def { _dropdownConfig_attributes = constDyn ("class" =: "form-select") }
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
                        { _dropdownConfig_attributes = constDyn ("class" =: "form-select")
                        , _dropdownConfig_setValue = (ffor (updated dynNonDuplicateDropdownList) $ \opts -> fst $ Map.elemAt 0 opts)
                        }
                      coinBAmountInput <- inputElement $ def
                        & inputElementConfig_elementConfig . elementConfig_initialAttributes
                          .~ ("class" =: "form-control" <> "type" =: "number")
                        & inputElementConfig_initialValue
                          .~ ("0" :: Text)
                      return (_dropdown_value coinBChoice, _inputElement_value coinBAmountInput)
                    btnEnabled <- holdDyn True $ leftmost [False <$ swap, True <$ readyForRequest]
                    swap <- divClass "input-group row mt-3" $ do
                      (e,_) <- elDynAttr' "button" (toggleBtnUsability <$> btnEnabled) $ do
                        elDynAttr "span" (toggleSpinner <$> btnEnabled) blank
                        dyn_ $ ffor btnEnabled $ \case
                          True -> text "Swap"
                          False -> text "Loading..."
                      return $ domEvent Click e
                    let ffor4 a b c d f = liftA3 f a b c <*> d
                        pooledTokenToCoin pt = Coin $ AssetClass (CurrencySymbol (_pooledToken_symbol pt), TokenName (_pooledToken_name pt))
                        toAmount amt = Amount $ (read (T.unpack amt) :: Integer)
                        requestLoad = Api_Swap
                          <$> (constDyn $ ContractInstanceId wid)
                          <*> (pooledTokenToCoin <$> selectionA)
                          <*> (pooledTokenToCoin <$> selectionB)
                          <*> (toAmount <$> amountA)
                          <*> (toAmount <$> amountB)
                    -- This response returns transaction fee information from the swap response
                    responseVal <- requestingIdentity $ tagPromptlyDyn requestLoad swap
                    do
                        let
                          observableStateEvent :: Event t (Either Aeson.Value ())
                          observableStateEvent = void <$> responseVal
                          observableStateSuccessEvent :: Event t ()
                          observableStateSuccessEvent = fmapMaybe (preview _Right) observableStateEvent
                        -- this event will cause the success message to disappear when it occurs
                        vanishEvent <- delay 7 observableStateSuccessEvent
                        -- show success message based on new observable state
                        widgetHold_ blank $ ffor
                          (leftmost [True <$ observableStateSuccessEvent, False <$ vanishEvent]) $
                          \case
                            True -> blank
                            False -> elClass "p" "text-success" $ text "Success!"
                        widgetHold_ blank $ ffor (fmapMaybe (preview _Left) observableStateEvent) $ \errJson ->
                          elClass "p" "text-danger" $ text $ case errJson ^? _String of
                            Just errMsg -> errMsg
                            Nothing -> T.pack $ show errJson
                    let readyForRequest = () <$ responseVal
                    return $ ffor4 selectionA amountA selectionB amountB $ \selA amtA selB amtB -> Just ((selA, amtA), (selB, amtB))
              _ -> do
                elClass "p" "text-warning" $ text "There are no tokens available to swap."
                pure $ pure Nothing

transactionDetails
  :: forall t m
  .  MonadRhyoliteWidget (DexV (Const SelectedCount)) Api t m
  => Dynamic t (Maybe (Maybe Aeson.Value))
  -> Dynamic t (Maybe ((PooledToken, Text), (PooledToken, Text)))
  -> m ()
transactionDetails pabDMMV formDyn = do
      -- widget that shows transaction details such as swap estimates, etc.
      divClass "col" $ divClass "card mb-4 box-shadow h-100" $ do
        divClass "card-header" $ elClass "h4" "my-0 font-weight-normal" $ text "Transaction Details"
        divClass "card-body" $ do
          poolMapEv <- do
            let poolsEvent = wsFilterPools $ fmap join $ updated $ pabDMMV
            dynPoolMap <- holdDyn Map.empty $ ffor poolsEvent $ \mIncomingPoolsWebSocketData -> do
              let poolDetails = case mIncomingPoolsWebSocketData of
                    Nothing -> V.empty
                    Just poolsWebSocketData -> poolsWebSocketData ^. key "contents" . key "Right" . key "contents" . _Array
              parseLiquidityTokensToMap poolDetails
            return $ fmap Just $ updated dynPoolMap
          -- combine events from from updates and smart contract pool data to perform swap estimates
          dynPoolMap :: Dynamic t (Maybe (Map.Map Text (Integer, ((Text, Integer), (Text, Integer)))))
            <- holdDyn Nothing poolMapEv
          let swapEstimate = ffor ((,) <$> dynPoolMap <*> formDyn) $ \case
                (Just poolMap, Just ((selA, amtA), (selB, amtB))) -> do
                  let ((coinAName, coinAPoolAmount), (coinBName, coinBPoolAmount)) = fromMaybe (("", 0), ("", 0))
                        $ fmap snd
                        $ headMay
                        $ Map.elems
                        $ Map.filter (\(_,((tknameA,_), (tknameB, _)))
                        -> tknameA == (_pooledToken_name selA) && tknameB == (_pooledToken_name selB)) poolMap
                  let amtA' :: Integer = fromMaybe 0 $ readMay $ T.unpack amtA
                      amtB' :: Integer = fromMaybe 0 $ readMay $ T.unpack amtB
                  Just $ if amtA' == 0
                    then (findSwapA coinBPoolAmount coinAPoolAmount amtB', coinAName)
                    else (findSwapA coinAPoolAmount coinBPoolAmount amtA', coinBName)
                _ -> Nothing
          txFeeEstimateResp <- requesting $ fmap (\sca -> Api_EstimateTransactionFee sca) $ SmartContractAction_Swap <$ updated formDyn
          dyn_ $ ffor swapEstimate $ \case
           Nothing -> blank
           Just (estimate, eTokenName) ->
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
     , MonadHold t m
     , MonadFix m
     , Reflex t
     )
  => m (Dynamic t (Maybe (Maybe [PooledToken])))
viewPooledTokens = do
  v <- (fmap.fmap.fmap) (getFirst . runIdentity) $ queryViewMorphism 1 $ constDyn $ vessel Q_PooledTokens . identityV
  holdUniqDyn v

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
