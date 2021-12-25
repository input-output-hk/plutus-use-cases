{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main(main) where

import Control.Monad.Except
import Data.Aeson
import Data.String (fromString)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp
import Servant
import Service as GamesService
import Types.Game

type GamesAPI = "games" :> Get '[JSON] [Game]
            :<|> "games" :> Capture "id" GameId :> Get '[JSON] Game
            :<|> "games" :> Capture "id" GameId :> ReqBody '[JSON] UpdateGameParams :> Put '[JSON] Game
            :<|> "games" :> Capture "id" GameId :> "score" :> ReqBody '[JSON] UpdateGameScore :> Put '[JSON] Game

data UpdateGameParams = UpdateGameParams
  { ugpSatus        :: !FixtureStatusShort
  } deriving Generic
instance FromJSON UpdateGameParams
instance ToJSON UpdateGameParams

data UpdateGameScore = UpdateGameScore
  { ugpTeam         :: !TeamId
  } deriving Generic
instance FromJSON UpdateGameScore
instance ToJSON UpdateGameScore

gamesAPI :: Proxy GamesAPI
gamesAPI = Proxy

gamesServer :: Server GamesAPI
gamesServer = games
      :<|> gameById
      :<|> сhangeGameState
      :<|> changeGameScore
  where
    games:: Handler [Game]
    games = do
      gamesE <- liftIO $ runExceptT $ GamesService.getGames
      case gamesE of
          Left e          -> throwError err500{errBody=fromString  e}
          Right gamesList -> return gamesList
    gameById:: GameId -> Handler Game
    gameById gameId = do
      gameM <- liftIO $ runExceptT (GamesService.getGameById gameId)
      case gameM of
        Left e     -> throwError err404{errBody=fromString e}
        Right game -> return game
    сhangeGameState:: GameId -> UpdateGameParams -> Handler Game
    сhangeGameState gameId updateParams = do
      gameE <- liftIO $ runExceptT $ updateGameState (ugpSatus updateParams) gameId
      case gameE of
        Left e     -> throwError err500{errBody=fromString e}
        Right game -> return game
    changeGameScore:: GameId -> UpdateGameScore -> Handler Game
    changeGameScore gameId updateParams = do
      gameE <- liftIO $ runExceptT $ updateGameScore (ugpTeam updateParams) gameId
      case gameE of
        Left e     -> throwError err500{errBody=fromString e}
        Right game -> return game

gamesApp :: Application
gamesApp = serve gamesAPI gamesServer

main :: IO ()
main = do
  _ <- runExceptT $ createInitialGames
  run 8081 gamesApp
