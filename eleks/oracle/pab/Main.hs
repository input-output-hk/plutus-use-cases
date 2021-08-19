{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Main(main) where

import           Control.Monad                       (void, forM, forever)
import           Control.Monad.Freer                 (Eff, Member, interpret, type (~>))
import qualified Control.Concurrent.STM              as STM
import           Control.Monad.Freer.Error           (Error)
import           Control.Monad.Freer.Extras.Log      (LogMsg)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (FromJSON (..), Result (..), ToJSON (..), genericToJSON, genericParseJSON
                                                     , defaultOptions, Options(..), decode, encode, parseJSON, fromJSON)
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
import           Contracts.MutualBet                 as MutualBet
import           Contracts.Oracle                    as Oracle
import           Wallet.Emulator.Types               (Wallet (..))
import qualified Data.ByteString.Char8               as B
import           Ledger                              (PubKeyHash(..), pubKeyHash, CurrencySymbol(..), pubKeyAddress)
import qualified Ledger.Value                        as Value
import           Ledger.Value                        (TokenName (..), Value)
import           Wallet.API                          (ownPubKey)
import           Wallet.Emulator.Types
import           Wallet.Types                        (ContractInstanceId (..))
import qualified Ledger.Typed.Scripts                as Scripts
import           Plutus.PAB.Monitoring.PABLogMsg     (PABMultiAgentMsg)
import           Pab.Game

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin MutualBetContracts) "Starting mutual bet"
    shutdown <- PAB.Server.startServerDebug

    let w1 = Wallet 1
        w2 = Wallet 2
        oracleWallet = Wallet 5
    w1Address <- pubKeyAddress <$> Simulator.handleAgentThread w1 ownPubKey

    games <- liftIO $ fromRight [] <$> getGames
    void $ forM games $ \game -> do
        let gameId = fixtureId $ fixture game
            team1Id = teamId $ home $ teams game
            team2Id = teamId $ away $ teams game
        
        cidOracleToken <- Simulator.activateContract oracleWallet $ OracleTokenInit
        currency <- waitForLast cidOracleToken
        let oracleParams = Oracle.OracleParams{ 
            Oracle.opFees = 1_000_000
            , Oracle.opGame = gameId
            , Oracle.opSymbol = Currency.currencySymbol currency
            }
        cidOracle <- Simulator.activateContract oracleWallet $ OracleСontract oracleParams
        oracle <- waitForLast cidOracle
        
        forM wallets $ \wallet -> do
            let mutualBetParams = MutualBet.MutualBetParams { 
                mbpGame = gameId
                , mbpOracle = oracle
                , mbpTeam1 = team1Id
                , mbpTeam2 = team2Id }
            Simulator.logString @(Builtin MutualBetContracts) $ "params"  ++ show mutualBetParams
            void $ Simulator.activateContract w1 $ MutualBetStartContract mutualBetParams

    Simulator.waitNSlots 10

    Simulator.logString @(Builtin MutualBetContracts) $ "Enter to continue"
    void $ liftIO getLine

    Simulator.logString @(Builtin MutualBetContracts) "Balances at the end of the simulation"
    b <- Simulator.currentBalances
    Simulator.logBalances @(Builtin MutualBetContracts) b

    shutdown

data MutualBetContracts =
    OracleTokenInit
    | MutualBetStartContract MutualBet.MutualBetParams
    | OracleСontract Oracle.OracleParams
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty MutualBetContracts where
    pretty = viaShow

instance HasDefinitions MutualBetContracts where
    getDefinitions = []
    getSchema = \case
        OracleTokenInit             -> Builtin.endpointsToSchemas @Empty
        MutualBetStartContract _    -> Builtin.endpointsToSchemas @MutualBet.MutualBetStartSchema
        OracleСontract _            -> Builtin.endpointsToSchemas @Oracle.OracleSchema
    getContract = \case
        OracleTokenInit               -> SomeBuiltin initContract
        MutualBetStartContract params -> SomeBuiltin $ MutualBet.mutualBetStart params
        OracleСontract params         -> SomeBuiltin $ Oracle.runOracle params

handlers :: SimulatorEffectHandlers (Builtin MutualBetContracts)
handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (contractHandler (Builtin.handleBuiltin @MutualBetContracts))

waitForLast :: FromJSON a => ContractInstanceId -> Simulator.Simulation t a
waitForLast cid =
    flip Simulator.waitForState cid $ \json -> case fromJSON json of
        Success (Last (Just x)) -> Just x
        _                       -> Nothing

wallets :: [Wallet]
wallets = [Wallet i | i <- [1 .. 4]]

initContract :: Contract (Last Currency.OneShotCurrency) Currency.CurrencySchema Currency.CurrencyError ()
initContract = do
    ownPK <- pubKeyHash <$> Contract.ownPubKey
    cur   <- Currency.mintContract ownPK [(Oracle.oracleTokenName, 1)]
    let cs = Currency.currencySymbol cur
    tell $ Last $ Just cur