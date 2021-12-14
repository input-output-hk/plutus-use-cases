{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}

module Services.GameClient
    ( getGames
    , getGameById
    ) where

import Contracts.Oracle
import Control.Concurrent
import Control.Exception
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Monoid (Last (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Network.HTTP.Req
import Types.Game

getGames :: IO (Either String [Game])
getGames = runReq defaultHttpConfig $ do
    r <- req
        GET
        (http "127.0.0.1" /: "games")
        NoReqBody
        jsonResponse
        (port 8081)
    if responseStatusCode r /= 200
        then return $ Left "error getting games"
        else do
            return $ case fromJSON $ responseBody r of
                Success games -> Right games
                _             -> Left "error decoding state"

getGameById :: GameId -> IO (Either String Game)
getGameById gameId = runReq defaultHttpConfig $ do
    r <- req
        GET
        (http "127.0.0.1" /: "games" /: (pack $ show gameId))
        NoReqBody
        jsonResponse
        (port 8081)
    if responseStatusCode r /= 200
        then return $ Left "error getting game by id "
        else do
            return $ case fromJSON $ responseBody r of
                Success game -> Right game
                _            -> Left "error decoding state"
