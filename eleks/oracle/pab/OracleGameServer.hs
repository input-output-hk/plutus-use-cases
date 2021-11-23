{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main
    ( main
    ) where

import           Data.Aeson                                 (fromJSON, ToJSON, encode, Result (..), Value)
import           Control.Concurrent
import           Control.Exception
import           Control.Monad                              (when, void, forM)
import           Control.Lens   
import qualified Data.ByteString.Lazy.Char8                 as B8
import           Control.Monad.IO.Class                     (MonadIO (..))
import           Data.ByteString                            (ByteString)
import           Data.Either                                (fromRight)
import           Data.ByteString.Char8                      (unpack)
import           Data.Proxy                                 (Proxy (..))
import           Data.Text                                  (Text, pack)
import           Data.Monoid                                (Last (..))
import           Data.UUID
import           Network.HTTP.Req
import           GameClient
import           PabContracts                               (MutualBetContracts, handlers)
import           Types.Game    
import           Contracts.Oracle
import           Plutus.PAB.Events.ContractInstanceState    (PartiallyDecodedResponse (..))
import           Plutus.PAB.Webserver.Types
import           Network.HTTP.Req
import qualified Data.ByteString.Lazy.Char8                 as B8
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
            Left err -> do
                putStrLn $ "error games load: " ++ show err
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
        
        -- let y = Just x
        -- when (m /= y) $
        --     updateOracle uuid x
        -- threadDelay 5_000_000
        -- go uuid y

updateOracle :: UUID -> Game -> IO ()
updateOracle uuid game = do
    let winnerId = fromRight 0 $ getWinnerTeamId game
    let gameId = game ^. fixture . fixtureId;
    let gameStatus = game ^. fixture . status . short
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
callEndpoint cid name a = handle h $ runReq defaultHttpConfig $ do
    liftIO $ printf "request body: %s\n\n" $ B8.unpack $ encode a
    v <- req
        POST
        (http "127.0.0.1" /: "api"  /: "contract" /: "instance" /: pack (show cid) /: "endpoint" /: pack name)
        (ReqBodyJson a)
        (Proxy :: Proxy (JsonResponse ()))
        (port 9080)
    when (responseStatusCode v /= 200) $
        liftIO $ ioError $ userError $ "error calling endpoint " ++ name
  where
    h :: HttpException -> IO ()
    h = ioError . userError . show