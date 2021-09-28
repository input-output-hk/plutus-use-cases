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

module Frontend.Pool
  ( poolDashboard
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

poolDashboard
  :: forall t m js
  .  ( MonadRhyoliteWidget (DexV (Const SelectedCount)) Api t m
     , Prerender js t m
     , MonadIO (Performable m)
     , SetRoute t (R FrontendRoute) m
     )
  => Text
  -> m ()
poolDashboard wid = do
  navBar' $ Just wid
  -- Widget to show liquidity pool blanance
  pb <- getPostBuild
  -- recurring event used to poll for pool balance
  pollingEvent <- tickLossyFromPostBuildTime 10
  -- give pab time to process incoming requests
  initPoolEvent <- delay 1 pb
  getPoolEvent <- delay 5 pollingEvent
  requesting_ $ (Api_CallFunds (ContractInstanceId wid)) <$ (leftmost [pb, () <$ pollingEvent])
  requesting_ $ (Api_CallPools (ContractInstanceId wid)) <$ (leftmost [initPoolEvent, () <$ getPoolEvent])
  walletStateUpdated <- fmap (switch . current) $ prerender (return never) $ do
    ws <- jsonWebSocket ("ws://localhost:8080/ws/" <> wid) (def :: WebSocketConfig t Aeson.Value)
    pure $ _webSocket_recv ws
  divClass "container" $ do
    divClass "pricing-header px-3 py-3 pt-md-5 pb-md-4 mx-auto text-center" $ do
      elClass "h1" "display-5 fw-bold" $ text "Pool Information"
      el "p" $ text "View your token pool liquidity balances, stake tokens, or redeem liquidity"
      -- incorporate the use of PAB's websockets to display the wallet's current Pool Balance
      -- filter for websocket events relevent to funds that contain the "Funds" tag and "NewObservableState" tag
      dFunds <- holdDyn Nothing $ wsFilterFunds walletStateUpdated
      dPools <- holdDyn Nothing $ wsFilterPools walletStateUpdated
      dyn_ $ ffor2 dFunds dPools $
        \(mIncomingFundsWebSocketData :: Maybe Aeson.Value) (mIncomingPoolsWebSocketData :: Maybe Aeson.Value) ->
          case mIncomingFundsWebSocketData of
            Nothing -> el "p" $ text "Stake Tokens to a Stake Pool in order to gain Liquidity"
            Just fundsWebSocketData -> do
              let currencyDetails = fundsWebSocketData ^. key "contents" . key "Right" . key "contents" . key "getValue" . _Array
                  poolDetails = case mIncomingPoolsWebSocketData of
                    Nothing -> V.empty
                    Just poolsWebSocketData -> poolsWebSocketData ^. key "contents" . key "Right" . key "contents" . _Array
                  poolMap = parseLiquidityTokensToMap poolDetails
                  formattedTokenDetails = Map.filter
                    -- Note: If token names change, this hash will need to change. Otherwise, the pool balances will not be found.
                    (\(_,cs) -> cs == "fe5cdcd31cf4e2facf00b6e9f0fa836adf3670f7c6c0e90cbd2c2b9719961f69") $
                    parseTokensToMap currencyDetails
              case Map.toList formattedTokenDetails of
                []-> blank
                _ -> elClass "table" "table" $ do
                    elClass "thead" "thead-primary" $ el "tr" $ do
                      elAttr "th" ("scope" =: "col") $ text "Pool Pair"
                      elAttr "th" ("scope" =: "col") $ text "Liquidity Balance"
                      elAttr "th" ("scope" =: "col") $ text "Liquidity Percentage"
                    el "tbody" $ do
                      forM_ (Map.toList formattedTokenDetails) $ \(tokenName, (tokenBalance,_)) -> do
                        let mLiquidityInfo = Map.lookup tokenName poolMap
                        case mLiquidityInfo of
                          Nothing -> blank
                          Just (lqTotal, ((tokenNameA, _),(tokenNameB,_))) -> do
                            let lqPercentage :: Double = ((fromIntegral tokenBalance) / (fromIntegral lqTotal)) * 100
                            el "tr" $ do
                              el "td" $ text $ (T.pack $ show $ if tokenNameA == "" then "Ada" else tokenNameA)
                                <> " and "
                                <> (T.pack $ show $ if tokenNameB == "" then "Ada" else tokenNameB)
                              el "td" $ text $ T.pack $ show tokenBalance
                              el "td" $ text $ T.pack $ show lqPercentage <> "%"
    -- Widget to redeem liquidity pool blanance
    divClass "card-group mb-3 text-center" $ do
      redeemDashboard wid walletStateUpdated
      stakeDashboard wid walletStateUpdated

redeemDashboard
  :: forall t m
  .  ( MonadRhyoliteWidget (DexV (Const SelectedCount)) Api t m
     , MonadIO (Performable m)
     )
  => Text
  -> Event t (Maybe Aeson.Value)
  -> m ()
redeemDashboard wid walletStateUpdated = do
      divClass "row row-cols-1 row-cols-md-2 g-4 mb-3" $ do
        redeemFormEvent <- divClass "col" $ divClass "card mb-4 box-shadow h-100" $ do
          divClass "card-header" $ elClass "h4" "my-0 font-weight-normal" $ text "Redeem Liquidity"
          divClass "card-body" $ do
            el "p" $ text "Which liquidity pool would you like to redeem and how much?"
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
                      (selA, selB, amt) <- divClass "form container" $ do
                        divClass "form-group" $ mdo
                          -- Select first token
                          selectionA <- divClass "input-group row" $ do
                            coinAChoice <- dropdown fstOpt (constDyn $ dropdownList) $
                              def { _dropdownConfig_attributes = constDyn ("class" =: "form-select col-md-1") }
                            return $ _dropdown_value coinAChoice
                          let noduplicateDropdownList = Map.filterWithKey (\k _ -> k /= fstOpt) dropdownList
                          dynNonDuplicateDropdownList <- holdDyn noduplicateDropdownList $ ffor (updated selectionA)
                            $ \choice -> Map.filterWithKey (\k _ -> k /= choice) dropdownList
                          -- Select second token
                          selectionB <- divClass "input-group row mt-3" $ do
                            coinBChoice <- dropdown sndOpt dynNonDuplicateDropdownList $ DropdownConfig
                              { _dropdownConfig_attributes = constDyn ("class" =: "form-select col-md-1")
                              , _dropdownConfig_setValue = (ffor (updated dynNonDuplicateDropdownList) $ \opts -> fst $ Map.elemAt 0 opts)
                              }
                            return $ _dropdown_value coinBChoice
                          -- Select amount
                          amount <- divClass "input-group row mt-3" $ do
                            coinBAmountInput <- inputElement $ def
                              & inputElementConfig_elementConfig . elementConfig_initialAttributes
                                .~ ("class" =: "form-control col-md-4" <> "type" =: "number")
                              & inputElementConfig_initialValue
                                .~ ("0" :: Text)
                            return $ _inputElement_value coinBAmountInput
                          -- toggle-able dynamic bool for use with disabling buttons after clicking
                          btnEnabled <- toggle True $ leftmost [redeem, () <$ observableStateEv]
                          redeem <- divClass "input-group row mt-3" $ do
                            (e,_) <- elDynAttr' "button" (toggleBtnUsability <$> btnEnabled) $ do
                              elDynAttr "span" (toggleSpinner <$> btnEnabled) blank
                              widgetHold_ (text "Redeem") $ ffor (updated btnEnabled) $ \case
                                True -> text "Redeem"
                                False -> text "Loading..."
                            return $ domEvent Click e
                          let pooledTokenToCoin pt = Coin $ AssetClass (CurrencySymbol (_pooledToken_symbol pt), TokenName (_pooledToken_name pt))
                              toAmount amt = Amount $ (read (T.unpack amt) :: Integer)
                              requestLoad = ((\w c1 c2 a -> Api_RedeemLiquidity w c1 c2 a)
                                <$> (constDyn $ ContractInstanceId wid)
                                <*> (pooledTokenToCoin <$> selectionA)
                                <*> (pooledTokenToCoin <$> selectionB)
                                <*> (toAmount <$> amount))
                          -- This response doesn't return anything useful, so it is thrown away
                          _ <- requesting $ tagPromptlyDyn requestLoad redeem
                          observableStateEv <- do
                            let observableStateSuccessEvent = flip ffilter walletStateUpdated $ \(mIncomingWebSocketData :: Maybe Aeson.Value )
                                  -> case mIncomingWebSocketData of
                                    Nothing -> False
                                    Just incomingWebSocketData -> do
                                      let newObservableStateTag = incomingWebSocketData ^.. key "tag" . _String
                                          swappedTag = incomingWebSocketData ^.. key "contents" . key "Right" . key "tag" . _String
                                      newObservableStateTag == ["NewObservableState"] && swappedTag == ["Removed"]
                                observableStateFailureEvent = flip ffilter walletStateUpdated $ \(mIncomingWebSocketData :: Maybe Aeson.Value )
                                  -> case mIncomingWebSocketData of
                                    Nothing -> False
                                    Just incomingWebSocketData -> do
                                      let newObservableStateTag = incomingWebSocketData ^.. key "tag" . _String
                                          failureMessageTag = incomingWebSocketData ^.. key "contents" . key "Left" . _String
                                      newObservableStateTag == ["NewObservableState"] && failureMessageTag /= []
                            -- this event will cause the success message to disappear when it occurs
                            vanishEvent <- delay 7 observableStateSuccessEvent
                            -- show success message based on new observable state
                            widgetHold_ blank $ ffor (leftmost [observableStateSuccessEvent, Nothing <$ vanishEvent]) $
                              \(mIncomingWebSocketData :: Maybe Aeson.Value) -> case mIncomingWebSocketData of
                                Nothing -> blank
                                Just _ -> elClass "p" "text-success" $ text "Success!"
                            widgetHold_ blank $ ffor observableStateFailureEvent $
                              \(mIncomingWebSocketData :: Maybe Aeson.Value) -> case mIncomingWebSocketData of
                                Nothing -> blank
                                Just incomingWebSocketData -> do
                                  let errMsg = incomingWebSocketData ^.. key "contents" . key "Left" . _String
                                  elClass "p" "text-danger" $ text $ T.concat errMsg
                            return $ leftmost [observableStateSuccessEvent, observableStateFailureEvent]
                          return (selectionA, selectionB, amount)
                      return $ fmap Just $ updated $ ffor3 selA selB amt $ \a b c -> (a, b, c)
                    _ -> do
                      elClass "p" "text-warning" $ text "There are no tokens available to redeem."
                      return never
        divClass "col" $ divClass "card mb-4 box-shadow h-100" $ do
          divClass "card-header" $ elClass "h4" "my-0 font-weight-normal" $ text "Redeem Transaction Details"
          divClass "card-body" $ do
            poolMapEv <- do
              let poolsEvent = wsFilterPools walletStateUpdated
              -- use pool map to calculate and display tokens to be redeemed when removing liquidity
              dynPoolMap <- holdDyn Map.empty $ ffor poolsEvent $ \mIncomingPoolsWebSocketData -> do
                let poolDetails = case mIncomingPoolsWebSocketData of
                      Nothing -> V.empty
                      Just poolsWebSocketData -> poolsWebSocketData ^. key "contents" . key "Right" . key "contents" . _Array
                parseLiquidityTokensToMap poolDetails
              return $ fmap Just $ updated dynPoolMap
            dynPoolMap <- holdDyn Nothing poolMapEv
            poolAndRedeemForm <- holdDyn (Nothing, Nothing) $ attachPromptlyDyn dynPoolMap redeemFormEvent
            let redeemEstimate = ffor poolAndRedeemForm $ \case
                  (Just poolMap, Just (selA, selB, amt)) -> do
                    let (liquidityPoolAmount, ((tknameA, coinAPoolAmount), (tknameB, coinBPoolAmount))) = fromMaybe (1,(("",0),("",0)))
                          $ headMay
                          $ Map.elems
                          $ flip Map.filter poolMap
                          $ \(_, ((nameA,_),(nameB,_))) -> nameA == (_pooledToken_name selA) && nameB == (_pooledToken_name selB)
                        redeemAmount :: Integer = fromMaybe 0 $ readMay $ T.unpack amt
                        (remainingLiqA, remainingLiqB) = calculateRemoval coinAPoolAmount coinBPoolAmount liquidityPoolAmount redeemAmount
                    ((tknameA, coinAPoolAmount - remainingLiqA), (tknameB, coinBPoolAmount - remainingLiqB))
                  _ -> (("",0),("", 0))
                tdPlaceholder = el "p" $ text "See liquidity redemption details here after selecting a pool and amount to redeem."
            divClass "p-3 mt-2" $ widgetHold_ tdPlaceholder $ ffor (updated redeemEstimate) $ \((tna, redeemableAmountA), (tnb, redeemableAmountB)) ->
              elClass "p" "text-info" $ text
                $ "When redeeming this liquidity amount, you are estimated to receive "
                <> (T.pack $ show redeemableAmountA)
                <> " "
                <> (T.pack $ show $ if tna == "" then "ADA" else tna)
                <> " and "
                <> (T.pack $ show redeemableAmountB)
                <> " "
                <> (T.pack $ show $ if tnb == "" then "ADA" else tnb)


stakeDashboard
  :: forall t m
  .  ( MonadRhyoliteWidget (DexV (Const SelectedCount)) Api t m
     , MonadIO (Performable m)
     )
  => Text
  -> Event t (Maybe Aeson.Value)
  -> m ()
stakeDashboard wid walletStateUpdated = do
    divClass "row row-cols-1 row-cols-md-2 g-4 mb-3" $ do
      -- Widget with form to allow user to stake/add to pool
      stakeFormEvent <- divClass "col" $ divClass "card mb-4 box-shadow h-100" $ do
          divClass "card-header" $ elClass "h4" "my-0 font-weight-normal" $ text "Stake Tokens"
          divClass "card-body" $ do
            el "p" $ text "What would you like to stake?"
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
                      divClass "form container" $ do
                        divClass "form-group" $ mdo
                          -- Select first token and amount
                          (selectionA, amountA) <- divClass "input-group row" $ do
                            coinAChoice <- dropdown fstOpt (constDyn $ dropdownList) $
                              def { _dropdownConfig_attributes = constDyn ("class" =: "form-select col-md-1") }
                            coinAAmountInput <- inputElement $ def
                              & inputElementConfig_elementConfig . elementConfig_initialAttributes
                                .~ ("class" =: "form-control col-md-4" <> "type" =: "number")
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
                                .~ ("class" =: "form-control col-md-4" <> "type" =: "number")
                              & inputElementConfig_initialValue
                                .~ ("0" :: Text)
                            return (_dropdown_value coinBChoice, _inputElement_value coinBAmountInput)
                          btnEnabled <- toggle True $ leftmost [stake, () <$ observableStateEv]
                          stake <- divClass "input-group row mt-3" $ do
                            (e,_) <- elDynAttr' "button" (toggleBtnUsability <$> btnEnabled) $ do
                              elDynAttr "span" (toggleSpinner <$> btnEnabled) blank
                              widgetHold_ (text "Stake") $ ffor (updated btnEnabled) $ \case
                                True -> text "Stake"
                                False -> text "Loading..."
                            return $ domEvent Click e
                          let pooledTokenToCoin pt = Coin $ AssetClass (CurrencySymbol (_pooledToken_symbol pt), TokenName (_pooledToken_name pt))
                              toAmount amt = Amount $ (read (T.unpack amt) :: Integer)
                              requestLoad = ((\w c1 c2 a1 a2 -> Api_Stake w c1 c2 a1 a2)
                                <$> (constDyn $ ContractInstanceId wid)
                                <*> (pooledTokenToCoin <$> selectionA)
                                <*> (pooledTokenToCoin <$> selectionB)
                                <*> (toAmount <$> amountA)
                                <*> (toAmount <$> amountB))
                              ffor4 a b c d f = liftA3 f a b c <*> d
                              poolSelectionAmounts = ffor4 selectionA amountA selectionB amountB $ \a b c d -> ((a,b),(c,d))
                          -- This response doesn't return anything useful, so it is thrown away
                          _ <- requesting $ tagPromptlyDyn requestLoad stake
                          observableStateEv <- do
                            let observableStateSuccessEvent = flip ffilter walletStateUpdated $ \(mIncomingWebSocketData :: Maybe Aeson.Value )
                                  -> case mIncomingWebSocketData of
                                    Nothing -> False
                                    Just incomingWebSocketData -> do
                                      let newObservableStateTag = incomingWebSocketData ^.. key "tag" . _String
                                          swappedTag = incomingWebSocketData ^.. key "contents" . key "Right" . key "tag" . _String
                                      newObservableStateTag == ["NewObservableState"] && swappedTag == ["Added"]
                                observableStateFailureEvent = flip ffilter walletStateUpdated $ \(mIncomingWebSocketData :: Maybe Aeson.Value )
                                  -> case mIncomingWebSocketData of
                                    Nothing -> False
                                    Just incomingWebSocketData -> do
                                      let newObservableStateTag = incomingWebSocketData ^.. key "tag" . _String
                                          failureMessageTag = incomingWebSocketData ^.. key "contents" . key "Left" . _String
                                      newObservableStateTag == ["NewObservableState"] && failureMessageTag /= []
                            -- this event will cause the success message to disappear when it occurs
                            vanishEvent <- delay 7 observableStateSuccessEvent
                            -- show success message based on new observable state
                            widgetHold_ blank $ ffor (leftmost [observableStateSuccessEvent, Nothing <$ vanishEvent]) $
                              \(mIncomingWebSocketData :: Maybe Aeson.Value) -> case mIncomingWebSocketData of
                                Nothing -> blank
                                Just _ -> elClass "p" "text-success" $ text "Success!"
                            widgetHold_ blank $ ffor observableStateFailureEvent $
                              \(mIncomingWebSocketData :: Maybe Aeson.Value) -> case mIncomingWebSocketData of
                                Nothing -> blank
                                Just incomingWebSocketData -> do
                                  let errMsg = incomingWebSocketData ^.. key "contents" . key "Left" . _String
                                  elClass "p" "text-danger" $ text $ T.concat errMsg
                            return $ leftmost [observableStateFailureEvent, observableStateSuccessEvent]
                          return $ fmap Just $ updated poolSelectionAmounts
                    _ -> do
                      elClass "p" "text-warning" $ text "There are no tokens available to stake."
                      return never
      divClass "col" $ divClass "card mb-4 box-shadow h-100" $ do
        divClass "card-header" $ elClass "h4" "my-0 font-weight-normal" $ text "Stake Transaction Details"
        divClass "card-body" $ do
          let poolsEvent = wsFilterPools walletStateUpdated
          -- use pool map to calculate and display tokens to be redeemed when removing liquidity
          dynPoolMap <- holdDyn Nothing $ fmap Just $ ffor poolsEvent $ \mIncomingPoolsWebSocketData -> do
            let poolDetails = case mIncomingPoolsWebSocketData of
                  Nothing -> V.empty
                  Just poolsWebSocketData -> poolsWebSocketData ^. key "contents" . key "Right" . key "contents" . _Array
            parseLiquidityTokensToMap poolDetails
          dynPoolAndForm <- holdDyn (Nothing, Nothing) $ attachPromptlyDyn dynPoolMap stakeFormEvent
          let liquidityEstimate = ffor dynPoolAndForm $ \case
                (Just poolMap, Just ((selA,amtA) ,(selB, amtB))) -> do
                  let (liquidityPoolAmount, ((_, coinAPoolAmount), (_, coinBPoolAmount))) = fromMaybe (1,(("",1),("",1)))
                        $ headMay
                        $ Map.elems
                        $ flip Map.filter poolMap
                        $ \(_, ((nameA,_),(nameB,_))) -> nameA == (_pooledToken_name selA) && nameB == (_pooledToken_name selB)
                      stakeAmountA :: Integer = fromMaybe 0 $ readMay $ T.unpack amtA
                      stakeAmountB :: Integer = fromMaybe 0 $ readMay $ T.unpack amtB
                  calculateAdditionalLiquidity coinAPoolAmount coinBPoolAmount liquidityPoolAmount stakeAmountA stakeAmountB
                _ -> 0
              tdPlaceholder = el "p" $ text "See staking details here after selecting and specifying the amount of tokens to be staked."
          divClass "p-3 mt-2" $ widgetHold_ tdPlaceholder $ ffor (updated liquidityEstimate) $ \liqEst ->
            elClass "p" "text-info" $ text
              $ "Staking this amount to the pool is estimated to yield "
              <> (T.pack $ show liqEst)
              <> " Liquidity Tokens"

      return ()

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
