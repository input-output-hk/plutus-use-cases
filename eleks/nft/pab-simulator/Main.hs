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
{-# LANGUAGE NumericUnderscores #-}

module Main(main) where

import           Control.Monad                       (void, forM)
import           Control.Monad.Freer                 (Eff, Member, interpret, type (~>))
import qualified Control.Concurrent.STM              as STM
import           Control.Monad.Freer.Error           (Error)
import           Control.Monad.Freer.Extras.Log      (LogMsg)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Default                        (Default (def))
import qualified Data.Monoid                         as Monoid
import qualified Data.Map.Strict                     as Map
import qualified Data.OpenApi.Schema                 as OpenApi
import           Data.Text                           (Text, pack)
import qualified Data.Text                           as T
import qualified Data.ByteString.Char8               as C
import           Data.Aeson                          (FromJSON (..), Result (..), ToJSON (..), genericToJSON, genericParseJSON
                                                     , defaultOptions, Options(..), decode, encode, parseJSON, fromJSON)
import           Prettyprinter                       (Pretty (..), viaShow)
import           GHC.Generics                        (Generic)
import           Plutus.Contract                     (ContractError)
import           Plutus.PAB.Effects.Contract         (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (..), SomeBuiltin (..), HasDefinitions (..), type (.\\))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Types                   (PABError, WebserverConfig (..), baseUrl, defaultWebServerConfig) 
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import           Plutus.PAB.Types                    (PABError (..))
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Contracts.NFT                       as NFTMarket
import           Wallet.Emulator                     (knownWallets, knownWallet) 
import           Wallet.Emulator.Types               (Wallet (..))
import qualified Data.ByteString.Char8               as B
import qualified Ledger.Value                        as Value
import           Ledger.Value                        (TokenName (..), Value)
import           Ledger                              (CurrencySymbol(..), pubKeyAddress)
import qualified Ledger.Typed.Scripts                as Scripts
import           Plutus.PAB.Monitoring.PABLogMsg     (PABMultiAgentMsg)
import           Servant.Client            (BaseUrl (..), Scheme (Http))

extract :: Maybe a -> a
extract (Just x) = x
extract Nothing  = undefined 

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin NFTMarketContracts) "Starting nft market place PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug' defaultWebServerConfig{ baseUrl = BaseUrl Http "localhost" 8080 ""}

    let w1 = knownWallet 1

    nftMarketInstance <- Simulator.activateContract w1 NFTStartContract
    Simulator.logString @(Builtin NFTMarketContracts) $ "URa"
    void $ Simulator.callEndpointOnInstance nftMarketInstance "start" ()
    Simulator.logString @(Builtin NFTMarketContracts) $ "URa1"
    Simulator.waitNSlots 10
    market       <- flip Simulator.waitForState nftMarketInstance $ \json -> case (fromJSON json :: Result (Monoid.Last (Either Text NFTMarket.NFTMarket))) of
                    Success (Monoid.Last (Just (Right market))) -> Just market
                    _                                             -> Nothing
    Simulator.logString @(Builtin NFTMarketContracts) $ "URa2"
    Simulator.logString @(Builtin NFTMarketContracts) $ "NFT Marketplace instance created: " ++ show market
    cids <- fmap Map.fromList $ forM walletsIndexes $ \wIndex -> do
        let w = knownWallet wIndex
        cid <- Simulator.activateContract w $ NFTUserContract market
        liftIO $ writeFile ('W' : show (wIndex) ++ ".cid") $ show cid
        Simulator.logString @(Builtin NFTMarketContracts) $ "NFT user contract started for " ++ show w
        return (w, cid)

    Simulator.waitNSlots 10
    Simulator.logString @(Builtin NFTMarketContracts) $ "Enter to continue"
    void $ liftIO getLine
    void $ liftIO getLine
    Simulator.logString @(Builtin NFTMarketContracts) "Balances at the end of the simulation"
    b <- Simulator.currentBalances
    Simulator.logBalances @(Builtin NFTMarketContracts) b
    shutdown

data NFTMarketContracts =
      NFTStartContract 
    | NFTUserContract NFTMarket.NFTMarket
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty NFTMarketContracts where
    pretty = viaShow

instance HasDefinitions NFTMarketContracts where
    getDefinitions = []
    getSchema = \case
        NFTStartContract -> Builtin.endpointsToSchemas @NFTMarket.MarketOwnerSchema
        NFTUserContract _ -> Builtin.endpointsToSchemas @NFTMarket.MarketUserSchema
    getContract = \case
        NFTStartContract -> SomeBuiltin (NFTMarket.ownerEndpoint NFTMarket.forgeMarketToken nftMarketFee)
        NFTUserContract market -> SomeBuiltin (NFTMarket.userEndpoints market)

nftMarketFee :: Integer
nftMarketFee = 2_000_000

handlers :: SimulatorEffectHandlers (Builtin NFTMarketContracts)
handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (contractHandler (Builtin.handleBuiltin @NFTMarketContracts))

walletsIndexes :: [Integer]
walletsIndexes = [1 .. 4]