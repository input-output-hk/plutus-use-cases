{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Main(main) where

import           Control.Monad                       (void)
import           Control.Monad.Freer                 (Eff, Member, interpret, type (~>))
import qualified Control.Concurrent.STM                         as STM
import           Control.Monad.Freer.Error           (Error)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (FromJSON (..), Result (..), ToJSON (..), genericToJSON, genericParseJSON
                                                     , defaultOptions, Options(..), decode, parseJSON, fromJSON)
import           Data.Text.Prettyprint.Doc           (Pretty (..), viaShow)
import           GHC.Generics                        (Generic)
import           Plutus.Contract                     (BlockchainActions, ContractError)
import           Plutus.PAB.Effects.Contract         (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), type (.\\))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import           Plutus.PAB.Types                    (PABError (..))
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Contracts.Currency                  as Currency
import           Contracts.Auction                   as Auction
import           Wallet.Emulator.Types               (Wallet (..))
import qualified Data.ByteString.Char8               as B
import qualified Ledger.Value            as Value
import           Ledger.Value                        (TokenName (..), Value)
import           Wallet.API                               (ownPubKey)
import           Ledger                                   (CurrencySymbol(..), pubKeyAddress)
import qualified Ledger.Typed.Scripts         as Scripts

extract :: Maybe a -> a
extract (Just x) = x          -- Sure, this works, but...
extract Nothing  = undefined 

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin StarterContracts) "Starting plutus-starter PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    --------------------------
    let w1 = Wallet 1
    let w2 = Wallet 2
    let tokenName = TokenName $ B.pack "TestCurrency"
    let tokenName1 = TokenName $ B.pack "TestCurrency"
    w1Address <- pubKeyAddress <$> Simulator.handleAgentThread w1 ownPubKey
    currencyInstance1 <- Simulator.activateContract w1 CurrencyContract
    void $ Simulator.waitForEndpoint currencyInstance1 "createNativeToken"
    void $ Simulator.callEndpointOnInstance currencyInstance1 "createNativeToken" SimpleMPS {Currency.tokenName = tokenName, amount = 1}
    nftTokenSymbol       <- flip Simulator.waitForState currencyInstance1 $ \json -> case fromJSON json of
                    Success (Just cur) -> Just $ Currency.currencySymbol cur
                    _                                             -> Nothing
    Simulator.logString @(Builtin StarterContracts) $ "Initialization finished. Minted NFT token: " ++ show nftTokenSymbol
   
    currencyInstance11 <- Simulator.activateContract w1 CurrencyContract
    void $ Simulator.callEndpointOnInstance currencyInstance11 "createNativeToken" SimpleMPS {Currency.tokenName = tokenName1, amount = 1}
    nftTokenSymbol1       <- flip Simulator.waitForState currencyInstance11 $ \json -> case fromJSON json of
                    Success (Just cur) -> Just $ Currency.currencySymbol cur
                    _                                             -> Nothing
    Simulator.logString @(Builtin StarterContracts) $ "Initialization finished. Minted NFT token 2: " ++ show nftTokenSymbol1
    
    auctionInstance1 <- Simulator.activateContract w1 AuctionContract
    auctionInstance2 <- Simulator.activateContract w2 AuctionContract
    currentSlotStm <- Simulator.currentSlot 
    currentSlot <- liftIO $ STM.atomically currentSlotStm
    let deadlineSlot = currentSlot + 10
    void $ Simulator.waitForEndpoint auctionInstance1 "start"
    void $ Simulator.callEndpointOnInstance auctionInstance1 "start" StartParams { spDeadline = deadlineSlot, spMinBid = 100, spCurrency=nftTokenSymbol, spToken=tokenName}
    Simulator.waitNSlots 10

    void $ Simulator.waitForEndpoint auctionInstance2 "bid"
    void $ Simulator.callEndpointOnInstance auctionInstance2 "bid" BidParams { bpCurrency = nftTokenSymbol, bpToken=tokenName, bpBid = 1000 }
    Simulator.waitNSlots 10

    void $ Simulator.waitForEndpoint auctionInstance1 "close"
    void $ Simulator.callEndpointOnInstance auctionInstance1 "close" CloseParams { cpCurrency = nftTokenSymbol, cpToken=tokenName }
    Simulator.waitNSlots 1
    ------------
    void $ liftIO getLine
    
    Simulator.logString @(Builtin StarterContracts) "Balances at the end of the simulation"
    b <- Simulator.currentBalances
    Simulator.logBalances @(Builtin StarterContracts) b

    shutdown

data StarterContracts =
    CurrencyContract 
    | AuctionContract 
    deriving (Eq, Ord, Show, Generic)

instance ToJSON StarterContracts where
  toJSON = genericToJSON defaultOptions {
             tagSingleConstructors = True }
instance FromJSON StarterContracts where
  parseJSON = genericParseJSON defaultOptions {
             tagSingleConstructors = True }

instance Pretty StarterContracts where
    pretty = viaShow

handleStarterContract ::
    ( Member (Error PABError) effs
    )
    => ContractEffect (Builtin StarterContracts)
    ~> Eff effs
handleStarterContract = Builtin.handleBuiltin getSchema getContract where
    getSchema = \case
        CurrencyContract -> Builtin.endpointsToSchemas @(Currency.CurrencySchema .\\ BlockchainActions)
        AuctionContract -> Builtin.endpointsToSchemas @(Auction.AuctionSchema .\\ BlockchainActions)
    getContract = \case
        CurrencyContract -> SomeBuiltin Currency.forgeCurrency
        AuctionContract -> SomeBuiltin Auction.endpoints

handlers :: SimulatorEffectHandlers (Builtin StarterContracts)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin StarterContracts) [CurrencyContract, AuctionContract]
    $ interpret handleStarterContract

