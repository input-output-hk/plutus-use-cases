{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Plutus.PAB.Simulation where

import           Control.Monad                                (forM, forM_,
                                                               void, when)
import           Control.Monad.Freer                          (Eff, Member,
                                                               interpret,
                                                               type (~>))
import           Control.Monad.Freer.Error                    (Error)
import           Control.Monad.Freer.Extras.Log               (LogMsg)
import           Control.Monad.IO.Class                       (MonadIO (..))
import           Data.Aeson                                   (FromJSON,
                                                               Result (..),
                                                               ToJSON, encode,
                                                               fromJSON)
import qualified Data.Map.Strict                              as Map
import qualified Data.Monoid                                  as Monoid
import qualified Data.Semigroup                               as Semigroup
import           Data.Text                                    (Text)
import           Data.Text.Prettyprint.Doc                    (Pretty (..),
                                                               viaShow)
import           GHC.Generics                                 (Generic)
import           Ledger
import           Ledger.Ada                                   (adaSymbol,
                                                               adaToken,
                                                               adaValueOf,
                                                               lovelaceValueOf)
import           Ledger.Constraints
import qualified Ledger.Constraints.OffChain                  as Constraints
import qualified Ledger.Typed.Scripts                         as Scripts
import           Ledger.Value                                 as Value
import           Plutus.Abstract.ContractResponse             (ContractResponse (..))
import           Plutus.Contract                              hiding (when)
import           Plutus.Contracts.Currency                    as Currency
import qualified Plutus.Contracts.NftMarketplace.Endpoints    as Marketplace
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Marketplace
import qualified Plutus.Contracts.Services.Sale               as Sale
import           Plutus.PAB.Effects.Contract                  (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin          (Builtin,
                                                               SomeBuiltin (..),
                                                               type (.\\))
import qualified Plutus.PAB.Effects.Contract.Builtin          as Builtin
import           Plutus.PAB.Monitoring.PABLogMsg              (PABMultiAgentMsg)
import           Plutus.PAB.Simulator                         (Simulation,
                                                               SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                         as Simulator
import           Plutus.PAB.Types                             (PABError (..))
import qualified Plutus.PAB.Webserver.Server                  as PAB.Server
import           Prelude                                      hiding (init)
import           Wallet.Emulator.Types                        (Wallet (..),
                                                               walletPubKey)
import           Wallet.Types                                 (ContractInstanceId)
import           Data.Default                        (Default (def))

ownerWallet :: Wallet
ownerWallet = Wallet 1

userWallets :: [Wallet]
userWallets = [Wallet i | i <- [2 .. 4]]

data ContractIDs = ContractIDs { cidUser :: Map.Map Wallet ContractInstanceId, cidInfo :: ContractInstanceId }

activateContracts :: Simulation (Builtin MarketplaceContracts) ContractIDs
activateContracts = do
    cidStart <- Simulator.activateContract ownerWallet MarketplaceStart
    _  <- Simulator.callEndpointOnInstance cidStart "start" ()
    mp <- flip Simulator.waitForState cidStart $ \json -> case (fromJSON json :: Result (ContractResponse Text Marketplace.OwnerContractState)) of
        Success (CrSuccess (Marketplace.Started mp)) -> Just mp
        _                                            -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Marketplace instance created: " ++ show mp

    cidInfo <- Simulator.activateContract ownerWallet $ MarketplaceInfo mp

    users <- fmap Map.fromList $ forM userWallets $ \w -> do
        cid <- Simulator.activateContract w $ MarketplaceUser mp
        Simulator.logString @(Builtin MarketplaceContracts) $ "Marketplace user contract started for " ++ show w
        return (w, cid)

    pure $ ContractIDs users cidInfo

startMpServer :: IO ()
startMpServer = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin MarketplaceContracts) "Starting NFT Marketplace PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug
    _ <- activateContracts
    Simulator.logString @(Builtin MarketplaceContracts) "NFT Marketplace PAB webserver started on port 8080. Initialization complete. Press enter to exit."
    _ <- liftIO getLine
    shutdown

runNftMarketplace :: IO ()
runNftMarketplace = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin MarketplaceContracts) "Starting Marketplace PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug
    ContractIDs {..} <- activateContracts
    let userCid = cidUser Map.! Wallet 2
        sender = pubKeyHash . walletPubKey $ Wallet 2
    let catTokenIpfsCid = "QmPeoJnaDttpFrSySYBY3reRFCzL3qv4Uiqz376EBv9W16"
    let photoTokenIpfsCid = "QmeSFBsEZ7XtK7yv5CQ79tqFnH9V2jhFhSSq1LV5W3kuiB"

    _  <-
        Simulator.callEndpointOnInstance userCid "createNft" $
            Marketplace.CreateNftParams {
                        cnpIpfsCid        = catTokenIpfsCid,
                        cnpNftName        = "Cat token",
                        cnpNftDescription = "A picture of a cat on a pogo stick",
                        cnpNftCategory = ["GIFs"],
                        cnpRevealIssuer   = False
                    }
    flip Simulator.waitForState userCid $ \json -> case (fromJSON json :: Result (ContractResponse Text Marketplace.UserContractState)) of
        Success (CrSuccess Marketplace.NftCreated) -> Just ()
        _                                          -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Successful createNft"

    _  <-
        Simulator.callEndpointOnInstance userCid "openSale" $
            Marketplace.OpenSaleParams {
                    ospItemId   = Marketplace.UserNftId catTokenIpfsCid,
                    ospSalePrice = 44*oneAdaInLovelace
                }
    sale <- flip Simulator.waitForState userCid $ \json -> case (fromJSON json :: Result (ContractResponse Text Marketplace.UserContractState)) of
        Success (CrSuccess Marketplace.OpenedSale) -> Just ()
        _                                          -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Successful openSale"

    let buyerCid = cidUser Map.! Wallet 3
        buyer = pubKeyHash . walletPubKey $ Wallet 3

    _  <-
        Simulator.callEndpointOnInstance buyerCid "buyItem" Marketplace.CloseLotParams {
                                                                clpItemId   = Marketplace.UserNftId catTokenIpfsCid
                                                            }
    _ <- flip Simulator.waitForState buyerCid $ \json -> case (fromJSON json :: Result (ContractResponse Text Marketplace.UserContractState)) of
        Success (CrSuccess Marketplace.NftBought) -> Just ()
        _                                         -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Successful buyItem"

    _  <-
        Simulator.callEndpointOnInstance userCid "createNft" $
            Marketplace.CreateNftParams {
                        cnpIpfsCid        = photoTokenIpfsCid,
                        cnpNftName        = "Photo token",
                        cnpNftDescription = "A picture of a sunset",
                        cnpNftCategory = ["Photos"],
                        cnpRevealIssuer   = True
                    }
    flip Simulator.waitForState userCid $ \json -> case (fromJSON json :: Result (ContractResponse Text Marketplace.UserContractState)) of
        Success (CrSuccess Marketplace.NftCreated) -> Just ()
        _                                          -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Successful createNft"

    _  <-
        Simulator.callEndpointOnInstance userCid "openSale" $
            Marketplace.OpenSaleParams {
                    ospItemId   = Marketplace.UserNftId photoTokenIpfsCid,
                    ospSalePrice = 12*oneAdaInLovelace
                }
    sale <- flip Simulator.waitForState userCid $ \json -> case (fromJSON json :: Result (ContractResponse Text Marketplace.UserContractState)) of
        Success (CrSuccess Marketplace.OpenedSale) -> Just ()
        _                                          -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Successful openSale"

    _  <-
        Simulator.callEndpointOnInstance userCid "closeSale"
            Marketplace.CloseLotParams {
                    clpItemId   = Marketplace.UserNftId photoTokenIpfsCid
                }
    sale <- flip Simulator.waitForState userCid $ \json -> case (fromJSON json :: Result (ContractResponse Text Marketplace.UserContractState)) of
        Success (CrSuccess Marketplace.ClosedSale) -> Just ()
        _                                          -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Successful closeSale"

    let auction = Marketplace.StartAnAuctionParams {
                        saapItemId  = Marketplace.UserNftId photoTokenIpfsCid,
                        saapDuration = 80
                    }
    _  <-
        Simulator.callEndpointOnInstance userCid "startAnAuction" auction
    _ <- flip Simulator.waitForState userCid $ \json -> case (fromJSON json :: Result (ContractResponse Text Marketplace.UserContractState)) of
        Success (CrSuccess Marketplace.AuctionStarted) -> Just ()
        _                                              -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Started An Auction"

    _  <-
        Simulator.callEndpointOnInstance buyerCid "bidOnAuction" Marketplace.BidOnAuctionParams {
                                                                        boapItemId = Marketplace.UserNftId photoTokenIpfsCid,
                                                                        boapBid     = fromInteger $ 15*oneAdaInLovelace
                                                                    }
    _ <- flip Simulator.waitForState buyerCid $ \json -> case (fromJSON json :: Result (ContractResponse Text Marketplace.UserContractState)) of
        Success (CrSuccess Marketplace.BidSubmitted) -> Just ()
        _                                            -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Successful bidOnAuction"

    _ <- Simulator.callEndpointOnInstance cidInfo "getAuctionState" $ Marketplace.UserNftId photoTokenIpfsCid
    s <- flip Simulator.waitForState cidInfo $ \json -> case (fromJSON json :: Result (ContractResponse Text Marketplace.InfoContractState)) of
            Success (CrSuccess (Marketplace.AuctionState s)) -> Just s
            _                                                -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Final auction state: " <> show s

    _  <-
        Simulator.callEndpointOnInstance buyerCid "completeAnAuction" $ Marketplace.CloseLotParams $ Marketplace.UserNftId photoTokenIpfsCid
    _ <- flip Simulator.waitForState buyerCid $ \json -> case (fromJSON json :: Result (ContractResponse Text Marketplace.UserContractState)) of
        Success (CrSuccess Marketplace.AuctionComplete) -> Just ()
        _                                               -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Successful holdAnAuction"

    _  <-
        Simulator.callEndpointOnInstance userCid "bundleUp" $
            Marketplace.BundleUpParams {
                        bupIpfsCids        = [photoTokenIpfsCid,catTokenIpfsCid],
                        bupName        = "Picture gallery",
                        bupDescription = "Collection of visual media",
                        bupCategory = ["User","Stan"]
                    }
    flip Simulator.waitForState userCid $ \json -> case (fromJSON json :: Result (ContractResponse Text Marketplace.UserContractState)) of
        Success (CrSuccess Marketplace.Bundled) -> Just ()
        _                                       -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Successful bundleUp"

    _  <-
        Simulator.callEndpointOnInstance userCid "unbundle" $
            Marketplace.UnbundleParams {
                        upIpfsCids        = [photoTokenIpfsCid,catTokenIpfsCid]
                    }
    flip Simulator.waitForState userCid $ \json -> case (fromJSON json :: Result (ContractResponse Text Marketplace.UserContractState)) of
        Success (CrSuccess Marketplace.Unbundled) -> Just ()
        _                                         -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Successful unbundle"

    _ <- Simulator.callEndpointOnInstance cidInfo "fundsAt" buyer
    v <- flip Simulator.waitForState cidInfo $ \json -> case (fromJSON json :: Result (ContractResponse Text Marketplace.InfoContractState)) of
            Success (CrSuccess (Marketplace.FundsAt v)) -> Just v
            _                                           -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Final buyer funds: " <> show v

    _ <- Simulator.callEndpointOnInstance cidInfo "marketplaceStore" ()
    marketplaceStore <- flip Simulator.waitForState cidInfo $ \json -> case (fromJSON json :: Result (ContractResponse Text Marketplace.InfoContractState)) of
            Success (CrSuccess (Marketplace.MarketplaceStore marketplaceStore)) -> Just marketplaceStore
            _                                                  -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Final marketplaceStore: " <> show marketplaceStore

    _ <- Simulator.callEndpointOnInstance cidInfo "marketplaceFunds" ()
    v <- flip Simulator.waitForState cidInfo $ \json -> case (fromJSON json :: Result (ContractResponse Text Marketplace.InfoContractState)) of
            Success (CrSuccess (Marketplace.MarketplaceFunds v)) -> Just v
            _                                                    -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Final marketplace funds: " <> show v

    _ <- Simulator.callEndpointOnInstance cidInfo "fundsAt" sender
    v <- flip Simulator.waitForState cidInfo $ \json -> case (fromJSON json :: Result (ContractResponse Text Marketplace.InfoContractState)) of
            Success (CrSuccess (Marketplace.FundsAt v)) -> Just v
            _                                           -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Final user funds: " <> show v

    _ <- liftIO getLine
    shutdown

data MarketplaceContracts =
    MarketplaceStart
    | MarketplaceInfo Marketplace.Marketplace
    | MarketplaceUser Marketplace.Marketplace
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty MarketplaceContracts where
    pretty = viaShow

instance Builtin.HasDefinitions MarketplaceContracts where
    getDefinitions = [MarketplaceStart]
    getSchema = \case
        MarketplaceUser _          -> Builtin.endpointsToSchemas @Marketplace.MarketplaceUserSchema
        MarketplaceInfo _          -> Builtin.endpointsToSchemas @Marketplace.MarketplaceInfoSchema
        MarketplaceStart           -> Builtin.endpointsToSchemas @Marketplace.MarketplaceOwnerSchema
    getContract = \case
        MarketplaceInfo marketplace       -> SomeBuiltin . awaitPromise $ Marketplace.infoEndpoints marketplace
        MarketplaceUser marketplace       -> SomeBuiltin . awaitPromise $ Marketplace.userEndpoints marketplace
        MarketplaceStart           -> SomeBuiltin . awaitPromise $ Marketplace.ownerEndpoints

handlers :: SimulatorEffectHandlers (Builtin MarketplaceContracts)
handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (Builtin.contractHandler (Builtin.handleBuiltin @MarketplaceContracts))

oneAdaInLovelace :: Integer
oneAdaInLovelace = 1000000
