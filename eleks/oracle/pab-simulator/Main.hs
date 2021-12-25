{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Main(main) where

import Contracts.MutualBet
import Contracts.Oracle
import Control.Lens
import Control.Monad (forM, void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (Result (..), fromJSON)
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (..))
import Data.Text (Text)
import Ledger (PaymentPrivateKey (..), PaymentPubKeyHash (..))
import Ledger.CardanoWallet qualified as CW
import PabContracts.SimulatorPabContracts (MutualBetContracts (..), handlers)
import Plutus.PAB.Effects.Contract.Builtin (Builtin)
import Plutus.PAB.Simulator qualified as Simulator
import Plutus.PAB.Webserver.Server qualified as PAB.Server
import Services.GameClient qualified as GameClient
import Types.Game
import Wallet.Emulator.Types
import Wallet.Emulator.Wallet (walletToMockWallet)
import Wallet.Types (ContractInstanceId (..))

initGame :: Oracle -> Game -> Simulator.Simulation (Builtin MutualBetContracts) ()
initGame oracle game = do
    let gameId  = game ^. fixture . fixtureId
        team1Id = game ^. teams . home . teamId
        team2Id = game ^. teams . away . teamId

    let mutualBetStartParams = MutualBetStartParams
                            { mbspGame   = gameId
                            , mbspOracle = oracle
                            , mbspOwner  = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash mutualBetOwnerWallet
                            , mbspTeam1  = team1Id
                            , mbspTeam2  = team2Id
                            , mbspMinBet = 2_000_000
                            , mbspBetFee = 2_000_000 }
    Simulator.logString @(Builtin MutualBetContracts) $ "activate mutual bet contract for wallet " ++ show mutualBetOwnerWallet ++ " gameId " ++ show gameId
    Simulator.logString @(Builtin MutualBetContracts) $ "params" ++ show mutualBetStartParams
    cidStartContract <- Simulator.activateContract mutualBetOwnerWallet $ MutualBetStartContract mutualBetStartParams
    Simulator.logString @(Builtin MutualBetContracts) $ "start params"
    mutualBetParams:: MutualBetParams <- flip Simulator.waitForState cidStartContract $ \json -> case (fromJSON json :: Result (Last (Either Text MutualBetParams))) of
                    Success (Last (Just (Right params))) -> Just params
                    _                                    -> Nothing
    Simulator.logString @(Builtin MutualBetContracts) $ "mutual bet params " ++ (show mutualBetParams)
    void $ forM bettorWallets $ \bettorWallet -> do
        Simulator.logString @(Builtin MutualBetContracts) $ "activate bettor contract for wallet " ++ show bettorWallet ++ " gameId " ++ show gameId
        cidBettorContract <- Simulator.activateContract bettorWallet $ MutualBetBettorContract mutualBetParams
        Simulator.waitForEndpoint cidBettorContract "bet"
        Simulator.logString @(Builtin MutualBetContracts) $ "bettor endpoint started for wallet " ++ show bettorWallet ++ " gameId " ++ show gameId
        return ()
    return ()

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do

    Simulator.logString @(Builtin MutualBetContracts) "Starting mutual bet"
    shutdown <- PAB.Server.startServerDebug

    let oracleParams = OracleParams
                        {
                          opFees   = 3_000_000
                        , opSigner = encodeKeyToDto $ unPaymentPrivateKey oraclePrivateKey
                        }
    cidOracle <- Simulator.activateContract oracleWallet $ OracleContract oracleParams
    liftIO $ writeFile "oracle.cid" $ show $ unContractInstanceId cidOracle
    oracle <- waitForLastOracle cidOracle
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

waitForLastOracle :: ContractInstanceId -> Simulator.Simulation t Oracle
waitForLastOracle cid =
    flip Simulator.waitForState cid $ \json -> case fromJSON json of
    Success (Last (Just (OracleState state))) -> Just state
    _                                         -> Nothing

bettorWallets :: [Wallet]
bettorWallets = take 4 knownWallets

mutualBetOwnerWallet :: Wallet
mutualBetOwnerWallet = knownWallet 6

oracleWallet :: Wallet
oracleWallet = knownWallet 5

oraclePrivateKey :: PaymentPrivateKey
oraclePrivateKey = CW.paymentPrivateKey $ fromMaybe(error $ "priv key") $ walletToMockWallet oracleWallet
