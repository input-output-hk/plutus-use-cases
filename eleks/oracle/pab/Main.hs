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

module Main(main) where

import           Control.Monad                       (void, forM)
import           Control.Monad.Freer                 (Eff, Member, interpret, type (~>))
import qualified Control.Concurrent.STM              as STM
import           Control.Monad.Freer.Error           (Error)
import           Control.Monad.Freer.Extras.Log      (LogMsg)
import           Control.Monad.IO.Class              (MonadIO (..))
import qualified Data.Monoid                         as Monoid
import qualified Data.Map.Strict                     as Map
import           Data.Text                           (Text, pack)
import qualified Data.Text                           as T
import qualified Data.ByteString.Char8               as C
import           Data.Aeson                          (FromJSON (..), Result (..), ToJSON (..), genericToJSON, genericParseJSON
                                                     , defaultOptions, Options(..), decode, encode, parseJSON, fromJSON)
import           Data.Default                        (Default (def))
import           Data.Text.Prettyprint.Doc           (Pretty (..), viaShow)
import           GHC.Generics                        (Generic)
import           Plutus.Contract                     (ContractError, awaitPromise, Promise (..))
import           Plutus.PAB.Effects.Contract         (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (..), SomeBuiltin (..), HasDefinitions (..), type (.\\))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import           Plutus.PAB.Types                    (PABError (..))
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Contracts.MutualBet                 as MutualBet
import           Wallet.Emulator.Types               (Wallet (..))
import qualified Data.ByteString.Char8               as B
import qualified Ledger.Value                        as Value
import           Ledger.Value                        (TokenName (..), Value)
import           Wallet.API                          (ownPubKey)
import           Ledger                              (CurrencySymbol(..), pubKeyAddress)
import qualified Ledger.Typed.Scripts                as Scripts
import           Plutus.PAB.Monitoring.PABLogMsg     (PABMultiAgentMsg)


main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin MutualBetContracts) "Starting mutual bet"
    shutdown <- PAB.Server.startServerDebug

    let w1 = Wallet 1
    w1Address <- pubKeyAddress <$> Simulator.handleAgentThread w1 ownPubKey

    Simulator.waitNSlots 10
    Simulator.logString @(Builtin MutualBetContracts) $ "Enter to continue"
    void $ liftIO getLine

    Simulator.logString @(Builtin MutualBetContracts) "Balances at the end of the simulation"
    b <- Simulator.currentBalances
    Simulator.logBalances @(Builtin MutualBetContracts) b

    shutdown

data MutualBetContracts =
    MutualBetStartContract MutualBet.MutualBetParams
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty MutualBetContracts where
    pretty = viaShow

instance HasDefinitions MutualBetContracts where
    getDefinitions = []
    getSchema = \case
        MutualBetStartContract _ -> Builtin.endpointsToSchemas @MutualBet.MutualBetStartSchema
    getContract = \case
        MutualBetStartContract params -> SomeBuiltin $ MutualBet.mutualBetStart params

handlers :: SimulatorEffectHandlers (Builtin MutualBetContracts)
handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (contractHandler (Builtin.handleBuiltin @MutualBetContracts))

wallets :: [Wallet]
wallets = [Wallet i | i <- [1 .. 4]]