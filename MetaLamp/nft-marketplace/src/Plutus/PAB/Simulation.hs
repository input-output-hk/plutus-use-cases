{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
module Plutus.PAB.Simulation where

import           Control.Monad                                  (forM, forM_,
                                                                 void, when)
import           Control.Monad.Freer                            (Eff, Member,
                                                                 interpret,
                                                                 type (~>))
import           Control.Monad.Freer.Error                      (Error)
import           Control.Monad.Freer.Extras.Log                 (LogMsg)
import           Control.Monad.IO.Class                         (MonadIO (..))
import qualified Data.Aeson                                     as J
import           Data.Default                                   (Default (def))
import qualified Data.Map.Strict                                as Map
import           Data.Monoid                                    (Last (..))
import qualified Data.Monoid                                    as Monoid
import qualified Data.OpenApi.Schema                            as OpenApi
import           Data.Proxy                                     (Proxy (..))
import qualified Data.Semigroup                                 as Semigroup
import           Data.Text                                      (Text)
import           Data.Text.Prettyprint.Doc                      (Pretty (..),
                                                                 viaShow)
import qualified Data.Time.Clock                                as Time
import           Ext.Plutus.Ledger.Time                         (convertUtcToPOSIX)
import qualified Ext.Plutus.PAB.Webserver.Server                as Ext.Plutus.PAB
import           GHC.Generics                                   (Generic)
import           Ledger
import           Ledger.Ada                                     (adaSymbol,
                                                                 adaToken,
                                                                 adaValueOf,
                                                                 lovelaceValueOf)
import qualified Ledger.Ada                                     as Ada
import           Ledger.Constraints
import qualified Ledger.Constraints.OffChain                    as Constraints
import           Ledger.TimeSlot                                (SlotConfig (..))
import qualified Ledger.Typed.Scripts                           as Scripts
import           Ledger.Value                                   as Value
import           Playground.Types                               (SimulatorWallet (..),
                                                                 adaCurrency)
import           Plutus.Abstract.ContractResponse               (ContractResponse,
                                                                 ContractState (..))
import           Plutus.Abstract.RemoteData                     (RemoteData (..))
import           Plutus.Contract                                hiding (when)
import           Plutus.Contracts.Currency                      as Currency
import qualified Plutus.Contracts.NftMarketplace.Endpoints      as Marketplace
import qualified Plutus.Contracts.NftMarketplace.OffChain.Owner as Owner
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core   as Marketplace
import qualified Plutus.Contracts.Services.Sale                 as Sale
import           Plutus.PAB.Effects.Contract                    (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin            (Builtin,
                                                                 SomeBuiltin (..),
                                                                 type (.\\))
import qualified Plutus.PAB.Effects.Contract.Builtin            as Builtin
import           Plutus.PAB.Monitoring.PABLogMsg                (PABMultiAgentMsg)
import           Plutus.PAB.Simulator                           (Simulation,
                                                                 SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                           as Simulator
import           Plutus.PAB.Types                               (PABError (..))
import qualified Plutus.PAB.Types                               as PAB
import qualified Plutus.PAB.Webserver.Server                    as PAB
import           Plutus.V1.Ledger.Time                          (POSIXTime (..))
import           Prelude                                        hiding (init)
import           Wallet.Emulator.Types                          (WalletNumber (..),
                                                                 walletPubKey)
import           Wallet.Emulator.Wallet                         (Wallet (..),
                                                                 fromWalletNumber)
import           Wallet.Types                                   (ContractInstanceId)

ownerWallet :: Wallet
ownerWallet = fromWalletNumber $ WalletNumber 1

wallet2 :: Wallet
wallet2 = fromWalletNumber $ WalletNumber 2

wallet3 :: Wallet
wallet3 = fromWalletNumber $ WalletNumber 3

userWallets :: [Wallet]
userWallets = fromWalletNumber <$> [WalletNumber i | i <- [2 .. 4]]

startMarketplaceParams :: Owner.StartMarketplaceParams
startMarketplaceParams = Owner.StartMarketplaceParams {
    creationFee = 100000,  -- 0.1 ADA
    saleFee = (5, 2)
}

initialLotPrice :: Value.Value
initialLotPrice = lovelaceValueOf 100000000 -- 100 ADA

data ContractIDs = ContractIDs { cidUser :: Map.Map Wallet ContractInstanceId, cidInfo :: ContractInstanceId }

activateContracts :: Simulation (Builtin MarketplaceContracts) ContractIDs
activateContracts = do
    cidStart <- Simulator.activateContract ownerWallet MarketplaceStart
    _  <- Simulator.callEndpointOnInstance cidStart "start" startMarketplaceParams
    mp <- flip Simulator.waitForState cidStart $ \json -> case (J.fromJSON json :: J.Result (ContractResponse String Text Marketplace.OwnerContractState)) of
        J.Success (Last (Just (ContractState _ (Success (Marketplace.Started mp))))) -> Just mp
        _                                            -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Marketplace instance created: " ++ show mp

    cidInfo <- Simulator.activateContract ownerWallet $ MarketplaceInfo mp

    users <- fmap Map.fromList $ forM userWallets $ \w -> do
        cid <- Simulator.activateContract w $ MarketplaceUser mp
        Simulator.logString @(Builtin MarketplaceContracts) $ "Marketplace user contract started for " ++ show w
        return (w, cid)

    pure $ ContractIDs users cidInfo

startMpServer :: IO ()
startMpServer = do
    beginningOfTime <- convertUtcToPOSIX <$> Time.getCurrentTime
    void $ Simulator.runSimulationWith (handlers $ slotConfiguration beginningOfTime) $ do
        Simulator.logString @(Builtin MarketplaceContracts) "Starting NFT Marketplace PAB webserver on port 9080. Press enter to exit."
        shutdown <- Ext.Plutus.PAB.startServer

        _ <- activateContracts
        Simulator.logString @(Builtin MarketplaceContracts) "NFT Marketplace PAB webserver started on port 9080. Initialization complete. Press enter to exit."
        _ <- liftIO getLine
        shutdown

runNftMarketplace :: IO ()
runNftMarketplace =
    void $ Simulator.runSimulationWith (handlers def) $ do
    Simulator.logString @(Builtin MarketplaceContracts) "Starting Marketplace PAB webserver on port 9080. Press enter to exit."
    shutdown <- PAB.startServerDebug
    ContractIDs {..} <- activateContracts
    let userCid = cidUser Map.! wallet2
        sender = pubKeyHash . walletPubKey $ wallet2
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
    flip Simulator.waitForState userCid $ \json -> case (J.fromJSON json :: J.Result (ContractResponse String Text Marketplace.UserContractState)) of
        J.Success (Last (Just (ContractState _ (Success Marketplace.NftCreated)))) -> Just ()
        _                                          -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Successful createNft"

    _  <-
        Simulator.callEndpointOnInstance userCid "openSale" $
            Marketplace.OpenSaleParams {
                    ospItemId   = Marketplace.UserNftId catTokenIpfsCid,
                    ospSalePrice = 44*oneAdaInLovelace
                }
    sale <- flip Simulator.waitForState userCid $ \json -> case (J.fromJSON json :: J.Result (ContractResponse String Text Marketplace.UserContractState)) of
        J.Success (Last (Just (ContractState _ (Success Marketplace.OpenedSale)))) -> Just ()
        _                                          -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Successful openSale"

    let buyerCid = cidUser Map.! wallet3
        buyer = pubKeyHash . walletPubKey $ wallet3

    _  <-
        Simulator.callEndpointOnInstance buyerCid "buyItem" Marketplace.CloseLotParams {
                                                                clpItemId   = Marketplace.UserNftId catTokenIpfsCid
                                                            }
    _ <- flip Simulator.waitForState buyerCid $ \json -> case (J.fromJSON json :: J.Result (ContractResponse String Text Marketplace.UserContractState)) of
        J.Success (Last (Just (ContractState _ (Success Marketplace.NftBought)))) -> Just ()
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
    flip Simulator.waitForState userCid $ \json -> case (J.fromJSON json :: J.Result (ContractResponse String Text Marketplace.UserContractState)) of
        J.Success (Last (Just (ContractState _ (Success Marketplace.NftCreated)))) -> Just ()
        _                                          -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Successful createNft"

    _ <- Simulator.waitNSlots 10

    _  <-
        Simulator.callEndpointOnInstance userCid "openSale" $
            Marketplace.OpenSaleParams {
                    ospItemId   = Marketplace.UserNftId photoTokenIpfsCid,
                    ospSalePrice = 12*oneAdaInLovelace
                }
    sale <- flip Simulator.waitForState userCid $ \json -> case (J.fromJSON json :: J.Result (ContractResponse String Text Marketplace.UserContractState)) of
        J.Success (Last (Just (ContractState _ (Success Marketplace.OpenedSale)))) -> Just ()
        _                                          -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Successful openSale"

    _ <- Simulator.waitNSlots 10

    _  <-
        Simulator.callEndpointOnInstance userCid "closeSale"
            Marketplace.CloseLotParams {
                    clpItemId   = Marketplace.UserNftId photoTokenIpfsCid
                }
    sale <- flip Simulator.waitForState userCid $ \json -> case (J.fromJSON json :: J.Result (ContractResponse String Text Marketplace.UserContractState)) of
        J.Success (Last (Just (ContractState _ (Success Marketplace.ClosedSale)))) -> Just ()
        _                                          -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Successful closeSale"

    let auction = Marketplace.StartAnAuctionParams {
                        saapItemId  = Marketplace.UserNftId photoTokenIpfsCid,
                        saapEndTime = POSIXTime 1635308387
                    }
    _  <-
        Simulator.callEndpointOnInstance userCid "startAnAuction" auction
    _ <- flip Simulator.waitForState userCid $ \json -> case (J.fromJSON json :: J.Result (ContractResponse String Text Marketplace.UserContractState)) of
        J.Success (Last (Just (ContractState _ (Success Marketplace.AuctionStarted)))) -> Just ()
        _                                              -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Started An Auction"

    _  <-
        Simulator.callEndpointOnInstance buyerCid "bidOnAuction" Marketplace.BidOnAuctionParams {
                                                                        boapItemId = Marketplace.UserNftId photoTokenIpfsCid,
                                                                        boapBid     = fromInteger $ 15*oneAdaInLovelace
                                                                    }
    _ <- flip Simulator.waitForState buyerCid $ \json -> case (J.fromJSON json :: J.Result (ContractResponse String Text Marketplace.UserContractState)) of
        J.Success (Last (Just (ContractState _ (Success Marketplace.BidSubmitted)))) -> Just ()
        _                                            -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Successful bidOnAuction"

    _ <- Simulator.callEndpointOnInstance cidInfo "getAuctionState" $ Marketplace.UserNftId photoTokenIpfsCid
    s <- flip Simulator.waitForState cidInfo $ \json -> case (J.fromJSON json :: J.Result (ContractResponse String Text Marketplace.InfoContractState)) of
            J.Success (Last (Just (ContractState _ (Success (Marketplace.AuctionState s))))) -> Just s
            _                                                -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Final auction state: " <> show s

    _  <-
        Simulator.callEndpointOnInstance buyerCid "completeAnAuction" $ Marketplace.CloseLotParams $ Marketplace.UserNftId photoTokenIpfsCid
    _ <- flip Simulator.waitForState buyerCid $ \json -> case (J.fromJSON json :: J.Result (ContractResponse String Text Marketplace.UserContractState)) of
        J.Success (Last (Just (ContractState _ (Success Marketplace.AuctionComplete)))) -> Just ()
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
    flip Simulator.waitForState userCid $ \json -> case (J.fromJSON json :: J.Result (ContractResponse String Text Marketplace.UserContractState)) of
        J.Success (Last (Just (ContractState _ (Success Marketplace.Bundled)))) -> Just ()
        _                                       -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Successful bundleUp"

    _  <-
        Simulator.callEndpointOnInstance userCid "unbundle" $
            Marketplace.UnbundleParams {
                        upIpfsCids        = [photoTokenIpfsCid,catTokenIpfsCid]
                    }
    flip Simulator.waitForState userCid $ \json -> case (J.fromJSON json :: J.Result (ContractResponse String Text Marketplace.UserContractState)) of
        J.Success (Last (Just (ContractState _ (Success Marketplace.Unbundled)))) -> Just ()
        _                                         -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Successful unbundle"

    _ <- Simulator.callEndpointOnInstance cidInfo "fundsAt" buyer
    v <- flip Simulator.waitForState cidInfo $ \json -> case (J.fromJSON json :: J.Result (ContractResponse String Text Marketplace.InfoContractState)) of
            J.Success (Last (Just (ContractState _ (Success (Marketplace.FundsAt v))))) -> Just v
            _                                           -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Final buyer funds: " <> show v

    _ <- Simulator.callEndpointOnInstance cidInfo "marketplaceStore" ()
    marketplaceStore <- flip Simulator.waitForState cidInfo $ \json -> case (J.fromJSON json :: J.Result (ContractResponse String Text Marketplace.InfoContractState)) of
            J.Success (Last (Just (ContractState _ (Success (Marketplace.MarketplaceStore marketplaceStore))))) -> Just marketplaceStore
            _                                                  -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Final marketplaceStore: " <> show marketplaceStore

    _ <- Simulator.callEndpointOnInstance cidInfo "marketplaceFunds" ()
    v <- flip Simulator.waitForState cidInfo $ \json -> case (J.fromJSON json :: J.Result (ContractResponse String Text Marketplace.InfoContractState)) of
            J.Success (Last (Just (ContractState _ (Success (Marketplace.MarketplaceFunds v))))) -> Just v
            _                                                    -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Final marketplace funds: " <> show v

    _ <- Simulator.callEndpointOnInstance cidInfo "fundsAt" sender
    v <- flip Simulator.waitForState cidInfo $ \json -> case (J.fromJSON json :: J.Result (ContractResponse String Text Marketplace.InfoContractState)) of
            J.Success (Last (Just (ContractState _ (Success (Marketplace.FundsAt v))))) -> Just v
            _                                           -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Final user funds: " <> show v

    _ <- liftIO getLine
    shutdown

data MarketplaceContracts =
    MarketplaceStart
    | MarketplaceInfo Marketplace.Marketplace
    | MarketplaceUser Marketplace.Marketplace
    deriving (Eq, Show, Generic)
    deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

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

slotConfiguration :: POSIXTime -> SlotConfig
slotConfiguration beginningOfTime = SlotConfig
        { scSlotLength   = 1000
        , scSlotZeroTime = beginningOfTime
        }

handlers :: SlotConfig -> SimulatorEffectHandlers (Builtin MarketplaceContracts)
handlers slotConfig =
    Simulator.mkSimulatorHandlers def slotConfig
    $ interpret (Builtin.contractHandler (Builtin.handleBuiltin @MarketplaceContracts))

oneAdaInLovelace :: Integer
oneAdaInLovelace = 1000000
