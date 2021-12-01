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
    , updateGameScore
    , createInitialGames
    ) where

import           Control.Lens    
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.UTF8  as U
import           Data.Default               (Default (def))
import           Data.Either.Combinators    (maybeToRight)
import           Data.Text                  (Text, pack)
import           Data.List                  (find)
import           GHC.Generics               (Generic)
import           Types.Game

createInitialGames :: ExceptT String IO [Game]
createInitialGames = do
    games <- lift $ (eitherDecodeFileStrict "gameserver/fixture-template.json" :: IO (Either String [Game]))
    (liftEither games) >>= saveGames

saveGames :: [Game] -> ExceptT String IO [Game]
saveGames games = do
    liftIO $ writeFile "gameserver/fixture.json" $ U.toString $ encodePretty games
    liftIO $ return games

getGames :: ExceptT String IO [Game]
getGames = do
    games <- lift $ (eitherDecodeFileStrict "gameserver/fixture.json" :: IO (Either String [Game]))
    liftEither games

getGameById :: GameId -> ExceptT String IO Game
getGameById gameId = do
    getGames >>= liftEither . findGameById gameId

findGameById :: GameId -> [Game] -> Either String Game
findGameById gameId games = do
    maybeToRight "Game not found" $ find (\g -> gameId == g ^. fixture . fixtureId) games

updateGameScore :: TeamId -> GameId -> ExceptT String IO Game
updateGameScore teamId gameId =
    getGameById gameId
    >>= liftEither . addGameScore teamId 
    >>= updateGame

scoreValue :: Integer -> Integer
scoreValue preValue = preValue +1 

addGameScore :: TeamId -> Game -> Either String Game 
addGameScore teamIdParam game
    | game ^. fixture . status . short /= LIVE = Left "Error goal update. Game not active"
    | game ^. teams . home . teamId == teamIdParam = Right $ game & goals . teamHome .~ (game ^. goals . teamHome + 1)
    | game ^. teams . away . teamId == teamIdParam = Right $ game & goals . teamAway .~ (game ^. goals . teamAway + 1)
    | otherwise = Left "Error goal update"

updateGameState :: FixtureStatusShort -> GameId -> ExceptT String IO Game
updateGameState status gameId =
    getGameById gameId
    >>= liftEither . updateGameStatus status 
    >>= liftEither . updateGameWinner 
    >>= updateGame

updateGameStatus :: FixtureStatusShort -> Game -> Either String Game 
updateGameStatus newStatus game = do
    let currentStatus =  game ^. fixture . status . short
    when (not $ validateGameStatusChanges currentStatus newStatus) (Left $ "Invalid state change from " ++ show currentStatus ++ " to new " ++ show newStatus)
    return $ game & fixture . status .~ (createFixtureStatus newStatus)

updateGameWinner :: Game -> Either String Game 
updateGameWinner game
    | game ^. fixture . status . short /= FT = Right game 
    | game ^. goals . teamHome == game ^. goals . teamAway = Right game 
    | game ^. goals . teamHome > game ^. goals . teamAway = Right $ game & teams . home . winner .~ True
    | game ^. goals . teamHome < game ^. goals . teamAway = Right $ game & teams . away . winner .~ True
    | otherwise = Left "Error winner update"

updateGame :: Game -> ExceptT String IO Game
updateGame updatedGame = do
    games <- getGames
    let updatedGames = (flip map games) (\game -> 
                    if updatedGame == game
                    then updatedGame
                    else game ) 
    saveGames updatedGames
    return updatedGame