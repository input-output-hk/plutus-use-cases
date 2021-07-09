{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.WebsocketParse where

import Control.Lens
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens
import qualified Data.HashMap.Lazy as HMap
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text)
import qualified Data.Vector as V
import Reflex.Dom.Core

-- returns Map of currencySymbol (token amount, token name)
parseTokensToMap :: V.Vector Aeson.Value -> Map.Map Text (Integer, Text)
parseTokensToMap cd = mconcat $ catMaybes $ V.toList $ ffor cd $ \case
  Aeson.Array xs -> case V.toList xs of
    symbol:(Aeson.Array tokens):_ ->
      let currencySymbol = symbol ^. key "unCurrencySymbol" . _String
          tokens' = ffor tokens $ \case
            Aeson.Array xs' -> case V.toList xs' of
              tokenName:tokenAmount:_ -> Just
                ( tokenName ^. key "unTokenName" . _String
                , (fromMaybe 0 $ tokenAmount ^? _Integer, currencySymbol)
                )
              _ -> Nothing
            _ -> Nothing
      in Just $ Map.fromList $ catMaybes $ V.toList tokens'
    _ -> Nothing
  _ -> Just Map.empty

-- returns Map of TokenName (LiquidityAmount, ((CoinA, CoinAPoolAmount), (CoinB, CoinBPoolAmount)))
parseLiquidityTokensToMap :: V.Vector Aeson.Value -> Map.Map Text (Integer, ((Text,Integer), (Text, Integer)))
parseLiquidityTokensToMap cd = mconcat $ catMaybes $ V.toList $ ffor cd $ \case
  Aeson.Array xs -> case V.toList xs of
    coinA:coinB:(Aeson.Array liquidityCoin):_-> case V.toList liquidityCoin of
      (Aeson.Object obj):(Aeson.Number lqAmount):_ -> case HMap.toList obj of
        (_, Aeson.String lqTokenName):_ -> do
          let tna = coinA ^. nth 0 . key "unAssetClass" . values . key "unTokenName" . _String
              tnaPool = fromMaybe 0 $ coinA ^? nth 1 . _Integer
              tnb = coinB ^. nth 0 . key "unAssetClass" . values . key "unTokenName" . _String
              tnbPool = fromMaybe 0 $ coinB ^? nth 1 . _Integer
          Just $ Map.singleton lqTokenName ((fromMaybe 0 $ lqAmount ^? _Integer), ((tna, tnaPool),(tnb, tnbPool)))
        _ -> Nothing
      _ -> Nothing
    _ -> Nothing
  _ -> Just Map.empty

-- filter websocket events for "Funds" related events
wsFilterFunds :: Reflex t => Event t (Maybe Aeson.Value) -> Event t (Maybe Aeson.Value)
wsFilterFunds recv = flip ffilter recv $ \(mIncomingWebSocketData :: Maybe Aeson.Value) -> case mIncomingWebSocketData of
  Nothing -> False
  Just incomingWebSocketData -> do
    let observableStateTag = incomingWebSocketData ^.. key "tag" . _String
        fundsTag = incomingWebSocketData ^.. key "contents" . key "Right" . key "tag" . _String
    observableStateTag == ["NewObservableState"] && fundsTag == ["Funds"]

-- filter websocket events for "Pools" related events
wsFilterPools :: Reflex t => Event t (Maybe Aeson.Value) -> Event t (Maybe Aeson.Value)
wsFilterPools recv = flip ffilter recv $ \(mIncomingWebSocketData :: Maybe Aeson.Value) -> case mIncomingWebSocketData of
  Nothing -> False
  Just incomingWebSocketData -> do
    let observableStateTag = incomingWebSocketData ^.. key "tag" . _String
        poolsTag = incomingWebSocketData ^.. key "contents" . key "Right" . key "tag" . _String
    observableStateTag == ["NewObservableState"] && poolsTag == ["Pools"]
