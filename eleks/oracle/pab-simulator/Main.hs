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
import           Data.Maybe                          (fromMaybe)
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
import           Ledger.Crypto                       (PrivateKey, PubKey)
import qualified Ledger.Value                        as Value
import           Ledger.Value                        (TokenName (..), Value)
import           Ledger.TimeSlot                     (SlotConfig)
import           Wallet.Emulator.Types
import           Wallet.Types                        (ContractInstanceId (..))
import qualified Ledger.Typed.Scripts                as Scripts
import           Plutus.PAB.Monitoring.PABLogMsg     (PABMultiAgentMsg)
import qualified Services.GameClient                          as GameClient
import           Wallet.Emulator                     (Wallet (..), knownWallets, knownWallet) 
import           Wallet.Emulator.Types               (Wallet (..), walletPubKeyHash)
import qualified Data.OpenApi.Schema                 as OpenApi
import           Playground.Contract                 (ToSchema)
import           Wallet.Emulator.Wallet              (fromMockWallet, toMockWallet, ownPublicKey, emptyWalletState)
import           PabContracts.SimulatorPabContracts   (MutualBetContracts(..), handlers)

initGame :: Oracle -> Game -> Simulator.Simulation (Builtin MutualBetContracts) ()
initGame oracle game = do
    let gameId  = game ^. fixture . fixtureId
        team1Id = game ^. teams . home . teamId
        team2Id = game ^. teams . away . teamId
        
    let mutualBetParams = MutualBetParams 
                            { mbpGame   = gameId
                            , mbpOracle = oracle
                            , mbpOwner  = walletPubKeyHash mutualBetOwnerWallet
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
    -- cidOracleToken <- Simulator.activateContract oracleWallet $ OracleTokenInit
    -- Simulator.logString @(Builtin MutualBetContracts) "Uraa1"
    -- currency <- waitForLast cidOracleToken
    let oracleParams = OracleParams
                        { --opSymbol = "aa",
                          opFees   = 3_000_000
                        , opSigner = encodeKeyToDto $ oraclePrivateKey
                        , opCollateral = 2_000_000
                        }             
    cidOracle <- Simulator.activateContract oracleWallet $ OracleСontract oracleParams
    liftIO $ writeFile "oracle.cid" $ show $ unContractInstanceId cidOracle
    oracle <- waitForLastOracle cidOracle
    Simulator.waitForEndpoint cidOracle "update"
    games <- liftIO $ fromRight [] <$> GameClient.getGames
    void $ forM games $ \game -> do
        -- creates oracle request only for one last game without waitNSlots 1
        Simulator.waitNSlots 1
        initGame oracle game

    void $ liftIO getLine
    void $ liftIO getLine
    Simulator.logString @(Builtin MutualBetContracts) "Balances at the end of the simulation"
    b <- Simulator.currentBalances
    Simulator.logBalances @(Builtin MutualBetContracts) b
    shutdown

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
oraclePrivateKey = ownPrivateKey . fromMaybe (error "not a mock wallet") . emptyWalletState  $ oracleWallet

oraclePublicKey :: PubKey
oraclePublicKey = ownPublicKey . fromMaybe (error "not a mock wallet") . emptyWalletState  $ oracleWallet

slotCfg :: SlotConfig
slotCfg = def
