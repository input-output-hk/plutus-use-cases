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

updateGameState :: TeamId -> FixtureStatusShort -> GameId -> ExceptT String IO Game
updateGameState winnerId status gameId =
    getGameById gameId
    >>= liftEither . updateGameWinner winnerId 
    >>= liftEither . updateGameStatus status 
    >>= updateGame

updateGameStatus :: FixtureStatusShort -> Game -> Either String Game 
updateGameStatus newStatus game = do
    let currentStatus =  game ^. fixture . status . short
    when (validateGameStatusChanges currentStatus newStatus) (Left $ "Invalid state change from " ++ show currentStatus ++ " to new " ++ show newStatus)
    return $ game & fixture . status .~ (createFixtureStatus newStatus)

updateGameWinner :: TeamId -> Game -> Either String Game 
updateGameWinner teamIdParam game
    | game ^. teams . home . teamId == teamIdParam = Right $ game & teams . home . winner .~ True
    | game ^. teams . away . teamId == teamIdParam = Right $ game & teams . away . winner .~ True
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