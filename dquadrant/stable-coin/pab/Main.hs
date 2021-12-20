{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Main(main) where

import           Control.Monad                           (forM_, void, when)
import           Control.Monad.Freer                     (Eff, Member, interpret, type (~>))
import           Control.Monad.Freer.Error               (Error)
import           Control.Monad.Freer.Extras.Log          (LogMsg)
import           Control.Monad.IO.Class                  (MonadIO (..))
import           Data.Aeson                              (FromJSON (..), ToJSON (..), genericToJSON, genericParseJSON
                                                         , defaultOptions, Options(..), Result(Success),fromJSON, encode)
import           Data.Default                           (Default (def))

import qualified Data.Map.Strict                         as Map
import qualified Data.Monoid                             as Monoid
import qualified Data.Semigroup                          as Semigroup
import           Data.Text                               (Text)
import           Data.Text.Prettyprint.Doc               (Pretty (..), viaShow)
import           GHC.Generics                            (Generic)
import           Ledger.Ada                              (adaSymbol, adaToken, adaValueOf,lovelaceValueOf)
import           Plutus.Contract                         hiding (when)
import           Plutus.PAB.Effects.Contract             (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin     (Builtin, SomeBuiltin (..), type (.\\))
import qualified Plutus.PAB.Effects.Contract.Builtin     as Builtin
import           Plutus.PAB.Monitoring.PABLogMsg         (PABMultiAgentMsg)
import           Plutus.PAB.Simulator                    (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                    as Simulator
import           Plutus.PAB.Types                        (PABError (..))
import qualified Plutus.PAB.Webserver.Server             as PAB.Server
import           Prelude                                 hiding (init)
import           Wallet.Emulator.Types                   (Wallet (..), walletPubKey)
import           Wallet.Types                            (ContractInstanceId (..))
import           Ledger
import           Ledger.Constraints
import qualified Ledger.Value                            as Value
import qualified Plutus.Contracts.Currency               as Currency
import qualified Data.ByteString.Lazy                    as LB
import           PlutusTx.Ratio                          as Ratio
import qualified PlutusTx.Numeric                        as P

import Plutus.Contracts.Coins.CoinsStateMachine
import Plutus.Contracts.Coins.Types
import Plutus.Contracts.Coins.Endpoints

import Plutus.Contracts.Oracle.Core

import qualified Data.Aeson.Types as AesonTypes

wallets :: [Wallet]
wallets = [Wallet i | i <- [2 .. 4]]

stableCoinName :: TokenName
stableCoinName = "StableToken"

reserveCoinName :: TokenName
reserveCoinName = "ReserveToken"

main :: IO ()
main =  
    void $ Simulator.runSimulationWith handlers $ do 
    Simulator.logString @(Builtin StableContracts) "Starting plutus-starter PAB webserver on port 9080. Press enter to exit."
    shutdown  <- PAB.Server.startServerDebug

    cidOracle <- Simulator.activateContract (Wallet 1) OracleContract

    liftIO $ writeFile "oracle.cid" $ show $ unContractInstanceId cidOracle

    oracle    <- flip Simulator.waitForState cidOracle $ \json -> case (fromJSON json :: Result (Monoid.Last (Oracle))) of
                    Success (Monoid.Last (Just x)) -> Just x
                    _                              -> Nothing
    
    Simulator.logString @(Builtin StableContracts) $ "Started oracle contract" ++ show oracle

    let pkhOwner = pubKeyHash $ walletPubKey $ Wallet 2

    let bp =
          BankParam
            { stableCoinTokenName = stableCoinName,
            reserveCoinTokenName = reserveCoinName,
            minReserveRatio = P.zero,
            maxReserveRatio = 100 % 1,
            rcDefaultRate = 1000000,
            oracleParam = oracle,
            oracleAddr = oracleAddress oracle,
            bankCurrencyAsset = Value.assetClass adaSymbol adaToken,
            bankContractOwner = pkhOwner
            }

    w1cid <- Simulator.activateContract (Wallet 1) $ StableContract bp
    Simulator.logString @(Builtin StableContracts) "Contract starting by wallet 1"

    --TODO Remove integer from start end 
    let i = 1 :: Integer
    _ <- Simulator.callEndpointOnInstance w1cid "start" i

    forM_ wallets $ \w -> do
            cid <- Simulator.activateContract w  $ StableContract bp
            liftIO $ writeFile ('W' : show (getWallet w) ++ ".cid") $ show $ unContractInstanceId cid
    
    -- _ <- Simulator.callEndpointOnInstance cidOracle "update" (1000000::Integer)

    void $ liftIO getLine
    
    Simulator.logString @(Builtin StableContracts) "Balances at the end of the simulation"
    b <- Simulator.currentBalances
    Simulator.logBalances @(Builtin StableContracts) b

    shutdown

data StableContracts = StableContract BankParam | OracleContract
    deriving (Show, Generic)

instance ToJSON StableContracts where
  toJSON = genericToJSON defaultOptions {
             tagSingleConstructors = True }
instance FromJSON StableContracts where
  parseJSON = genericParseJSON defaultOptions {
             tagSingleConstructors = True }

instance Pretty StableContracts where
    pretty = viaShow

instance Builtin.HasDefinitions StableContracts where
    getDefinitions =[OracleContract]
    getSchema = \case
      OracleContract   -> Builtin.endpointsToSchemas @OracleSchema
      StableContract _ -> Builtin.endpointsToSchemas @BankStateSchema
    getContract = \case
      OracleContract           -> SomeBuiltin $ oracleContract
      StableContract bankParam -> SomeBuiltin $ coinsEndpoints bankParam

handlers :: SimulatorEffectHandlers (Builtin StableContracts)
handlers =
    Simulator.mkSimulatorHandlers def def 
    $ interpret (Builtin.contractHandler (Builtin.handleBuiltin @StableContracts))
