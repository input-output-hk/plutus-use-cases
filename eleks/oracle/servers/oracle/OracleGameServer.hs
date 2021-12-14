{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main
    ( main
    ) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad                              (when, void, forM)
import           Control.Lens   
import qualified Data.ByteString.Lazy.Char8                 as B8
import           Control.Monad.IO.Class                     (MonadIO (..))
import           Data.Aeson                                 (fromJSON, ToJSON, encode, Result (..))
import           Data.Either                                (fromRight)
import           Data.Proxy                                 (Proxy (..))
import           Data.Text                                  (pack)
import           Data.UUID
import           Network.HTTP.Req
import           Services.GameClient
import           PabContracts.SimulatorPabContracts          (MutualBetContracts(..))
import           Types.Game    
import           Contracts.Oracle
import           Plutus.PAB.Events.ContractInstanceState    (PartiallyDecodedResponse (..))
import           Plutus.PAB.Webserver.Types
import           Text.Printf                                (printf)

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