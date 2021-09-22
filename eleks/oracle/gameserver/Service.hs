{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Service
    ( getGames
    , getGameById
    , getWinnerTeamId
    , updateGameState
    , createInitialGames
    ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.UTF8   as U
import           Data.Default               (Default (def))
import           Data.Text                  (Text, pack)
import           Control.Lens    
import           GHC.Generics               (Generic)
import           Types.Game
import           Data.List                  (find)


createInitialGames :: IO ()
createInitialGames = do
    games <- (eitherDecodeFileStrict "gameserver/fixture-template.json" :: IO (Either String [Game]))
    case games of
        Left _ -> return ()
        Right games -> saveGames games

saveGames :: [Game] -> IO ()
saveGames games = do
    writeFile "gameserver/fixture.json" $ U.toString $ encodePretty games

getGames :: IO (Either String [Game])
getGames = do
    games <- (eitherDecodeFileStrict "gameserver/fixture.json" :: IO (Either String [Game]))
    return games

getGameById :: GameId -> IO (Maybe Game) 
getGameById gameId = do
    gamesE <- getGames
    let game = case gamesE of 
          Left _      -> Nothing
          Right games -> findGameById gameId games
    return game

findGameById :: GameId -> [Game] -> Maybe Game
findGameById gameId games = do
    find (\g -> gameId == g ^. fixture . fixtureId) games

updateGameState :: TeamId -> FixtureStatusShort -> GameId -> IO (Maybe Game)
updateGameState winnerId status gameId = do
    gameM <- getGameById gameId 
    let updatedGame = gameM >>= 
                updateGameWinner winnerId >>=         
                updateGameStatus status
    case updatedGame of 
        Nothing -> return Nothing
        Just game -> do 
            updateGame game
            return $ Just game

updateGameStatus :: FixtureStatusShort -> Game -> Maybe Game 
updateGameStatus statusShort game = Just $ game & fixture . status .~ (createFixtureStatus statusShort)

updateGameWinner :: TeamId -> Game -> Maybe Game 
updateGameWinner teamIdParam game
    | game ^. teams . home . teamId == teamIdParam = Just $ game & teams . home . winner .~ True
    | game ^. teams . away . teamId == teamIdParam = Just $ game & teams . away . winner .~ True
    | otherwise = Nothing

updateGame :: Game -> IO (Maybe Game)
updateGame updatedGame = do
    games <- getGames
    case games of 
        Left _ -> return Nothing
        Right games -> do
            let updatedGames = (flip map games) (\game -> 
                    if updatedGame == game
                    then updatedGame
                    else game ) 
            saveGames updatedGames
            return $ Just updatedGame 