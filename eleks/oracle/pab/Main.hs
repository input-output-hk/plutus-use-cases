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
{-# LANGUAGE StandaloneDeriving #-}

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
import qualified Data.ByteString.Char8               as B
import           Ledger                              (PubKeyHash(..), pubKeyHash, CurrencySymbol(..), pubKeyAddress)
import           Ledger.Crypto                       (PrivateKey, privateKey5)
import qualified Ledger.Value                        as Value
import           Ledger.Value                        (TokenName (..), Value)
import           Ledger.TimeSlot                     (SlotConfig)
import           Wallet.API                          (ownPubKey)
import           Wallet.Emulator.Types
import           Wallet.Types                        (ContractInstanceId (..))
import qualified Ledger.Typed.Scripts                as Scripts
import           Plutus.PAB.Monitoring.PABLogMsg     (PABMultiAgentMsg)
import qualified GameClient                           as GameClient
import           Wallet.Emulator                     (Wallet (..), knownWallets, knownWallet, walletPubKey) 
import qualified Data.OpenApi.Schema                 as OpenApi
import           Playground.Contract                 (ToSchema)

deriving instance OpenApi.ToSchema SlotConfig
deriving instance OpenApi.ToSchema ThreadToken

initGame :: Oracle -> Game -> Simulator.Simulation (Builtin MutualBetContracts) ()
initGame oracle game = do
    let gameId  = game ^. fixture . fixtureId
        team1Id = game ^. teams . home . teamId
        team2Id = game ^. teams . away . teamId
        
    let mutualBetParams = MutualBetParams 
                            { mbpGame   = gameId
                            , mbpOracle = oracle
                            , mbpOwner = pubKeyHash $ walletPubKey mutualBetOwnerWallet
                            , mbpTeam1  = team1Id
                            , mbpTeam2  = team2Id 
                            , mbpMinBet = 2_000_000
                            , mbpBetFee = 2_000_000 }
    Simulator.logString @(Builtin MutualBetContracts) $ "activate mutual bet contract for wallet " ++ show mutualBetOwnerWallet ++ " gameId " ++ show gameId
    Simulator.logString @(Builtin MutualBetContracts) $ "params" ++ show mutualBetParams
    cidStartContract <- Simulator.activateContract mutualBetOwnerWallet $ MutualBetStartContract mutualBetParams
    Simulator.logString @(Builtin MutualBetContracts) $ "get thread token"
    threadToken <- waitForLastBetOuput cidStartContract
    Simulator.logString @(Builtin MutualBetContracts) $ "game thread token " ++ show threadToken
    void $ forM bettorWallets $ \bettorWallet -> do
        Simulator.logString @(Builtin MutualBetContracts) $ "activate bettor contract for wallet " ++ show bettorWallet ++ " gameId " ++ show gameId
        cidBettorContract <- Simulator.activateContract bettorWallet $ MutualBetBettorContract slotCfg threadToken mutualBetParams
        Simulator.waitForEndpoint cidBettorContract "bet"
        Simulator.logString @(Builtin MutualBetContracts) $ "bettor endpoint started for wallet " ++ show bettorWallet ++ " gameId " ++ show gameId
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
                        , opSigner = oraclePrivateKey
                        , opCollateral = 1_000_000
                        }
    cidOracle <- Simulator.activateContract oracleWallet $ OracleСontract oracleParams
    oracle <- waitForLastOracle cidOracle
    Simulator.waitForEndpoint cidOracle "update"
    games <- liftIO $ fromRight [] <$> GameClient.getGames
    void $ forM games $ \game -> do
        -- creates oracle request only for one last game without waitNSlots 1
        Simulator.waitNSlots 1
        initGame oracle game

    forever $ do
        Simulator.logString @(Builtin MutualBetContracts) $ "query active games"
        cidAwaitOracleRequest <- Simulator.callEndpointOnInstance cidOracle "games" ()
        activeGamesIds <- waitForLastGameIds cidOracle   
        Simulator.logString @(Builtin MutualBetContracts) $ "loaded active games" ++ show activeGamesIds
        void $ forM activeGamesIds $ \gameId -> do
            gameM <- liftIO $ GameClient.getGameById gameId
            case gameM of 
                Left err -> do
                    Simulator.logString @(Builtin MutualBetContracts) $ "Game not found " ++ show err
                Right game -> do
                    Simulator.logString @(Builtin MutualBetContracts) $ "Run for game" ++ show gameId
                    let winnerId = fromRight 0 $ getWinnerTeamId game
                    let gameStatus = game ^. fixture . status . short 
                    let updateParams = UpdateOracleParams 
                                        { uoGameId   = gameId
                                        , uoWinnerId = winnerId
                                        , uoGameStatus = gameStatus
                                        }     
                    void $ Simulator.callEndpointOnInstance cidOracle "update" updateParams
                    void $ liftIO $ threadDelay 1_000_000

        Simulator.logString @(Builtin MutualBetContracts) $ "wait 10 seconds"

        -- todo query active games and create contract
        void $ liftIO $ threadDelay 10_000_000

data MutualBetContracts =
    OracleTokenInit
    | MutualBetStartContract MutualBetParams
    | MutualBetBettorContract SlotConfig ThreadToken MutualBetParams
    | OracleСontract OracleParams
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)
    
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
bettorWallets = take 4 knownWallets

mutualBetOwnerWallet :: Wallet
mutualBetOwnerWallet = knownWallet 6

oracleWallet :: Wallet 
oracleWallet = knownWallet 5

oraclePrivateKey :: PrivateKey
oraclePrivateKey = privateKey5

slotCfg :: SlotConfig
slotCfg = def

initContract :: Contract (Last Currency.OneShotCurrency) Currency.CurrencySchema Currency.CurrencyError ()
initContract = do
    ownPK <- pubKeyHash <$> Contract.ownPubKey
    cur   <- Currency.mintContract ownPK [("test", 1)]
    let cs = Currency.currencySymbol cur
    tell $ Last $ Just cur