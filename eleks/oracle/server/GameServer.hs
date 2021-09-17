{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main(main) where

import Data.Text
import Data.String (fromString)
import Servant
import Pab.Game as GameApi
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import Network.Wai.Handler.Warp

type GamesAPI = "games" :> Get '[JSON] [GameApi.Game]
            :<|> "games" :> Capture "id" Integer :> Get '[JSON] GameApi.Game

gamesAPI :: Proxy GamesAPI
gamesAPI = Proxy

gamesServer :: Server GamesAPI
gamesServer = games
      :<|> gameById
  where 
    games:: Handler [GameApi.Game]
    games = do
      gamesE <- liftIO $  GameApi.getGames
      case gamesE of 
          Left e -> throwError err500{errBody=fromString  e}
          Right games -> return games
    gameById:: Integer -> Handler GameApi.Game
    gameById id = do
      gameM <- liftIO (GameApi.getGameById id)
      case gameM of 
        Nothing -> throwError err404{errBody=fromString $ "Game not found " ++ show id}
        Just game -> return game

gamesApp :: Application
gamesApp = serve gamesAPI gamesServer

main :: IO ()
main = run 8081 gamesApp