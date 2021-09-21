{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Main(main) where

import           Control.Lens    
import           Control.Monad                       (void, forM, forever)
import           Control.Monad.Freer                 (Eff, Member, interpret, type (~>))
import           Control.Concurrent                  (threadDelay)
import qualified Control.Concurrent.STM              as STM
import           Control.Monad.Freer.Error           (Error)
import           Control.Monad.Freer.Extras.Log      (LogMsg)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (FromJSON (..), Result (..), ToJSON (..), genericToJSON, genericParseJSON
                                                     , defaultOptions, decode, encode, parseJSON, fromJSON)
import qualified Data.ByteString.Char8               as C
import           Data.Default                        (Default (def))
import           Data.Either                         (fromRight)
import           Data.Text                           (Text, pack)
import qualified Data.Text                           as T
import qualified Data.Map.Strict                     as Map
import qualified Data.Semigroup                      as Semigroup
import           Data.Monoid                         (Last (..))
import           Data.Text.Prettyprint.Doc           (Pretty (..), viaShow)
import           GHC.Generics                        (Generic)
import qualified Plutus.Contract                     as Contract
import           Plutus.Contract                     (ContractError, Contract, awaitPromise, Promise (..), tell)
import           Plutus.Contracts.Currency           as Currency
import           Plutus.PAB.Effects.Contract         (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, Empty, BuiltinHandler (..), SomeBuiltin (..), HasDefinitions (..), type (.\\))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import           Plutus.PAB.Types                    (PABError (..))
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Contracts.MutualBet                 
import           Contracts.Oracle
import           Types.Game    
import           Wallet.Emulator.Types               (Wallet (..))
import qualified Data.ByteString.Char8               as B
import           Ledger                              (PubKeyHash(..), pubKeyHash, CurrencySymbol(..), pubKeyAddress)
import qualified Ledger.Value                        as Value
import           Ledger.Value                        (TokenName (..), Value)
import           Ledger.TimeSlot                     (SlotConfig)
import           Wallet.API                          (ownPubKey)
import           Wallet.Emulator.Types
import           Wallet.Types                        (ContractInstanceId (..))
import qualified Ledger.Typed.Scripts                as Scripts
import           Plutus.PAB.Monitoring.PABLogMsg     (PABMultiAgentMsg)
import qualified GameClient                           as GameClient

initGame :: Oracle -> Game -> Simulator.Simulation (Builtin MutualBetContracts) ()
initGame oracle game = do
    let gameId  = game ^. fixture . fixtureId
        team1Id = game ^. teams . home . teamId
        team2Id = game ^. teams . away . teamId
        
    let mutualBetParams = MutualBetParams 
                            { mbpGame   = gameId
                            , mbpOracle = oracle
                            , mbpTeam1  = team1Id
                            , mbpTeam2  = team2Id }
    Simulator.logString @(Builtin MutualBetContracts) $ "params" ++ show mutualBetParams
    cidStartContract <- Simulator.activateContract mutualBetOwnerWallet $ MutualBetStartContract mutualBetParams
    return cidStartContract
    Simulator.logString @(Builtin MutualBetContracts) $ "get thread token"
    threadToken <- waitForLastBetOuput cidStartContract
    Simulator.logString @(Builtin MutualBetContracts) $ "game thread token " ++ show threadToken
    void $ forM bettorWallets $ \bettorWallet -> do
        cidBettorContract <- Simulator.activateContract bettorWallet $ MutualBetBettorContract slotCfg threadToken mutualBetParams
        Simulator.waitForEndpoint cidBettorContract "bet"
        Simulator.logString @(Builtin MutualBetContracts) "bettor endpoint started for wallet"
        return ()
    return ()

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin MutualBetContracts) "Starting mutual bet"
    shutdown <- PAB.Server.startServerDebug

    cidOracleToken <- Simulator.activateContract oracleWallet $ OracleTokenInit
    currency <- waitForLast cidOracleToken
    let oracleParams = OracleParams
                        { opSymbol = Currency.currencySymbol currency
                        , opFees   = 1_500_000
                        , opSigner = (walletPrivKey oracleWallet)
                        }
    cidOracle <- Simulator.activateContract oracleWallet $ OracleСontract oracleParams
    oracle <- waitForLastOracle cidOracle
    Simulator.waitForEndpoint cidOracle "update"
    games <- liftIO $ fromRight [] <$> GameClient.getGames
    void $ forM games $ \game -> do
        initGame oracle game

    forever $ do
        Simulator.logString @(Builtin MutualBetContracts) $ "query active games"
        cidAwaitOracleRequest <- Simulator.callEndpointOnInstance cidOracle "games" ()
        activeGamesIds <- waitForLastGameIds cidOracle   
        Simulator.logString @(Builtin MutualBetContracts) $ "loaded active games" ++ show activeGamesIds
        void $ forM activeGamesIds $ \gameId -> do
            game <- liftIO $ GameClient.getGameById gameId
            let winnerIdM = getWinnerTeamId game
            case winnerIdM of
                Nothing -> Simulator.logString @(Builtin MutualBetContracts) $ "Game is not finished"
                Just winnerId -> do
                    let updateParams = UpdateOracleParams 
                                        { uoGameId   = gameId
                                        , uoWinnerId = winnerId
                                        }     
                    void $ Simulator.callEndpointOnInstance cidOracle "update" updateParams
                    updatedGameId <- waitForOracleUpdated cidOracle   
                    Simulator.logString @(Builtin MutualBetContracts) $ "updated for " ++ show updatedGameId

        Simulator.logString @(Builtin MutualBetContracts) $ "wait 5 seconds"

        -- todo query active games and create contract
        void $ liftIO $ threadDelay 10_000_000

data MutualBetContracts =
    OracleTokenInit
    | MutualBetStartContract MutualBetParams
    | MutualBetBettorContract SlotConfig ThreadToken MutualBetParams
    | OracleСontract OracleParams
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty MutualBetContracts where
    pretty = viaShow

instance HasDefinitions MutualBetContracts where
    getDefinitions = []
    getSchema = \case
        OracleTokenInit               -> Builtin.endpointsToSchemas @Empty
        MutualBetStartContract _      -> Builtin.endpointsToSchemas @MutualBetStartSchema
        MutualBetBettorContract _ _ _ -> Builtin.endpointsToSchemas @BettorSchema
        OracleСontract _              -> Builtin.endpointsToSchemas @OracleSchema
    getContract = \case
        OracleTokenInit                   -> SomeBuiltin initContract
        MutualBetStartContract params     -> SomeBuiltin $ mutualBetStart params
        MutualBetBettorContract conf threadToken params -> SomeBuiltin $ mutualBetBettor conf threadToken params
        OracleСontract params             -> SomeBuiltin $ runOracle params

handlers :: SimulatorEffectHandlers (Builtin MutualBetContracts)
handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (contractHandler (Builtin.handleBuiltin @MutualBetContracts))

waitForLast :: FromJSON a => ContractInstanceId -> Simulator.Simulation t a
waitForLast cid =
    flip Simulator.waitForState cid $ \json -> case fromJSON json of
        Success (Last (Just x)) -> Just x
        _                       -> Nothing

-- waitForLastOracle :: ContractInstanceId -> Simulator.Simulation t (Last OracleContractState)
-- waitForLastOracle cid =
--     flip Simulator.waitForState cid $ \json -> case fromJSON json of
--     Success (Last (Just state)) -> Just state
--     _                           -> Nothing

waitForLastOracle :: ContractInstanceId -> Simulator.Simulation t Oracle
waitForLastOracle cid =
    flip Simulator.waitForState cid $ \json -> case fromJSON json of
    Success (Last (Just (OracleState state))) -> Just state
    _                                         -> Nothing

waitForOracleUpdated :: ContractInstanceId -> Simulator.Simulation t GameId
waitForOracleUpdated cid =
    flip Simulator.waitForState cid $ \json -> case fromJSON json of
    Success (Last (Just (Updated gameId))) -> Just gameId
    _                                      -> Nothing

waitForLastGameIds :: ContractInstanceId -> Simulator.Simulation t [GameId]
waitForLastGameIds cid =
    flip Simulator.waitForState cid $ \json -> case fromJSON json of
        Success (Last (Just (Games ids))) -> Just ids
        _                                 -> Nothing

waitForLastBetOuput :: ContractInstanceId -> Simulator.Simulation t ThreadToken
waitForLastBetOuput cid =
    flip Simulator.waitForState cid $ \json -> case (fromJSON json) :: Result MutualBetOutput of
        Success (output) -> getLast $ mutualBetThreadToken output
        _           -> Nothing

bettorWallets :: [Wallet]
bettorWallets = [Wallet i | i <- [1 .. 4]]

mutualBetOwnerWallet :: Wallet
mutualBetOwnerWallet = Wallet 6

oracleWallet :: Wallet 
oracleWallet = Wallet 5

slotCfg :: SlotConfig
slotCfg = def

initContract :: Contract (Last Currency.OneShotCurrency) Currency.CurrencySchema Currency.CurrencyError ()
initContract = do
    ownPK <- pubKeyHash <$> Contract.ownPubKey
    cur   <- Currency.mintContract ownPK [(oracleTokenName, 1)]
    let cs = Currency.currencySymbol cur
    tell $ Last $ Just cur