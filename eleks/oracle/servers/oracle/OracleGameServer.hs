{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main
    ( main
    ) where

import Contracts.Oracle
import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad (forM, void, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (Result (..), ToJSON, encode, fromJSON)
import Data.ByteString.Lazy.Char8 qualified as B8
import Data.Either (fromRight)
import Data.Proxy (Proxy (..))
import Data.Text (pack)
import Data.UUID
import Network.HTTP.Req
import PabContracts.SimulatorPabContracts (MutualBetContracts (..))
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse (..))
import Plutus.PAB.Webserver.Types
import Services.GameClient
import Text.Printf (printf)
import Types.Game

main :: IO ()
main = do
    uuid <- read <$> readFile "oracle.cid"
    putStrLn $ "oracle contract instance id: " ++ show uuid
    go uuid []
  where
    go :: UUID -> [Game] -> IO a
    go uuid prevGames = do
        currentGamesE <- getGames
        case currentGamesE of
            Left currentGameErr -> do
                putStrLn $ "error games load: " ++ show currentGameErr
                threadDelay 5_000_000
                go uuid prevGames
            Right currentGames -> do
                putStrLn $ "current games: "
                activeGamesIds <- getActiveGamesIds uuid
                putStrLn $ "active games: " ++ show activeGamesIds
                let activeGames = filter(\game -> elem (game ^. fixture . fixtureId) activeGamesIds) currentGames
                void $ forM activeGames $ \game -> do
                    updateOracle uuid game
                    threadDelay 1_000_000
                threadDelay 5_000_000
                go uuid prevGames

updateOracle :: UUID -> Game -> IO ()
updateOracle uuid game = do
    let winnerId = fromRight 0 $ getWinnerTeamId game
    let gameId = game ^. fixture . fixtureId;
    let gameStatus = game ^. fixture . status . short
    let updateParams = UpdateOracleParams
                        { uoGameId   = gameId
                        , uoWinnerId = winnerId
                        , uoGameStatus = gameStatus
                        }
    callEndpoint uuid "update" updateParams

getActiveGamesIds :: UUID -> IO ([GameId])
getActiveGamesIds cid = do
    callEndpoint cid "games" ()
    threadDelay 2_000_000
    go
  where
    go = do
        state <- getStatus cid
        case state of
            (Games ids) -> return ids
            _           -> go

getStatus :: UUID -> IO (OracleContractState)
getStatus cid = runReq defaultHttpConfig $ do
    w <- req
        GET
        (http "127.0.0.1" /: "api"  /: "contract" /: "instance" /: pack (show cid) /: "status")
        NoReqBody
        (Proxy :: Proxy (JsonResponse (ContractInstanceClientState MutualBetContracts)))
        (port 9080)
    --liftIO $ putStrLn $ (show) $ (fromJSON :: Value -> Result (OracleContractState)) $ observableState $ cicCurrentState $ responseBody w
    case fromJSON $ observableState $ cicCurrentState $ responseBody w of
        Success (state) -> return (state)
        _               -> liftIO $ ioError $ userError "error decoding state"

callEndpoint :: ToJSON a => UUID -> String -> a -> IO ()
callEndpoint cid enpointName a = handle h $ runReq defaultHttpConfig $ do
    liftIO $ printf "request body: %s\n\n" $ B8.unpack $ encode a
    v <- req
        POST
        (http "127.0.0.1" /: "api"  /: "contract" /: "instance" /: pack (show cid) /: "endpoint" /: pack enpointName)
        (ReqBodyJson a)
        (Proxy :: Proxy (JsonResponse ()))
        (port 9080)
    when (responseStatusCode v /= 200) $
        liftIO $ ioError $ userError $ "error calling endpoint " ++ enpointName
  where
    h :: HttpException -> IO ()
    h = ioError . userError . show
