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
import qualified Data.ByteString                              as BS
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
import           Plutus.V1.Ledger.Crypto                      (getPubKeyHash,
                                                               pubKeyHash)
import           Prelude                                      hiding (init)
import           Wallet.Emulator.Types                        (Wallet (..),
                                                               walletPubKey)
import           Wallet.Types                                 (ContractInstanceId)

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
        Success (ContractSuccess (Marketplace.Started mp)) -> Just mp
        _                                                  -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Marketplace instance created: " ++ show mp

    cidInfo <- Simulator.activateContract ownerWallet $ MarketplaceInfo mp

    users <- fmap Map.fromList $ forM userWallets $ \w -> do
        cid <- Simulator.activateContract w $ MarketplaceUser mp
        Simulator.logString @(Builtin MarketplaceContracts) $ "Marketplace user contract started for " ++ show w
        return (w, cid)

    pure $ ContractIDs users cidInfo

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
        Success (ContractSuccess Marketplace.NftCreated) -> Just ()
        _                                                -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Successful createNft"

    _  <-
        Simulator.callEndpointOnInstance userCid "openSale" $
            Marketplace.OpenSaleParams {
                    ospIpfsCid   = catTokenIpfsCid,
                    ospSalePrice = 44*oneAdaInLovelace
                }
    sale <- flip Simulator.waitForState userCid $ \json -> case (fromJSON json :: Result (ContractResponse Text Marketplace.UserContractState)) of
        Success (ContractSuccess Marketplace.OpenedSale) -> Just ()
        _                                                -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Successful openSale"

    let buyerCid = cidUser Map.! Wallet 3
        buyer = pubKeyHash . walletPubKey $ Wallet 3

    _  <-
        Simulator.callEndpointOnInstance buyerCid "buyNft" Marketplace.BuyNftParams {
                                                                bnpIpfsCid   = catTokenIpfsCid
                                                            }
    _ <- flip Simulator.waitForState buyerCid $ \json -> case (fromJSON json :: Result (ContractResponse Text Marketplace.UserContractState)) of
        Success (ContractSuccess Marketplace.NftBought) -> Just ()
        _                                               -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Successful buyNft"

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
        Success (ContractSuccess Marketplace.NftCreated) -> Just ()
        _                                                -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Successful createNft"

    _  <-
        Simulator.callEndpointOnInstance userCid "openSale" $
            Marketplace.OpenSaleParams {
                    ospIpfsCid   = photoTokenIpfsCid,
                    ospSalePrice = 12*oneAdaInLovelace
                }
    sale <- flip Simulator.waitForState userCid $ \json -> case (fromJSON json :: Result (ContractResponse Text Marketplace.UserContractState)) of
        Success (ContractSuccess Marketplace.OpenedSale) -> Just ()
        _                                                -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Successful openSale"

    _  <-
        Simulator.callEndpointOnInstance userCid "closeSale"
            Marketplace.CloseSaleParams {
                    cspIpfsCid   = photoTokenIpfsCid
                }
    sale <- flip Simulator.waitForState userCid $ \json -> case (fromJSON json :: Result (ContractResponse Text Marketplace.UserContractState)) of
        Success (ContractSuccess Marketplace.ClosedSale) -> Just ()
        _                                                -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Successful closeSale"

    let auction = Marketplace.HoldAnAuctionParams {
                        haapIpfsCid  = photoTokenIpfsCid,
                        haapDuration = 80
                    }
    _  <-
        Simulator.callEndpointOnInstance userCid "startAnAuction" auction
    _ <- flip Simulator.waitForState userCid $ \json -> case (fromJSON json :: Result (ContractResponse Text Marketplace.UserContractState)) of
        Success (ContractSuccess Marketplace.AuctionStarted) -> Just ()
        _                                                    -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Started An Auction"

    _  <-
        Simulator.callEndpointOnInstance buyerCid "bidOnAuction" Marketplace.BidOnAuctionParams {
                                                                        japIpfsCid = photoTokenIpfsCid,
                                                                        japBid     = fromInteger $ 15*oneAdaInLovelace
                                                                    }
    _ <- flip Simulator.waitForState buyerCid $ \json -> case (fromJSON json :: Result (ContractResponse Text Marketplace.UserContractState)) of
        Success (ContractSuccess Marketplace.BidSubmitted) -> Just ()
        _                                                  -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Successful bidOnAuction"

    _ <- Simulator.callEndpointOnInstance cidInfo "getAuctionState" photoTokenIpfsCid
    s <- flip Simulator.waitForState cidInfo $ \json -> case (fromJSON json :: Result (ContractResponse Text Marketplace.InfoContractState)) of
            Success (ContractSuccess (Marketplace.AuctionState s)) -> Just s
            _                                                      -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Final auction state: " <> show s

    _  <-
        Simulator.callEndpointOnInstance buyerCid "completeAnAuction" auction
    _ <- flip Simulator.waitForState buyerCid $ \json -> case (fromJSON json :: Result (ContractResponse Text Marketplace.UserContractState)) of
        Success (ContractSuccess Marketplace.AuctionComplete) -> Just ()
        _                                                     -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Successful holdAnAuction"

    _ <- Simulator.callEndpointOnInstance cidInfo "fundsAt" buyer
    v <- flip Simulator.waitForState cidInfo $ \json -> case (fromJSON json :: Result (ContractResponse Text Marketplace.InfoContractState)) of
            Success (ContractSuccess (Marketplace.FundsAt v)) -> Just v
            _                                                 -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Final buyer funds: " <> show v

    _ <- Simulator.callEndpointOnInstance cidInfo "marketplaceStore" ()
    marketplaceStore <- flip Simulator.waitForState cidInfo $ \json -> case (fromJSON json :: Result (ContractResponse Text Marketplace.InfoContractState)) of
            Success (ContractSuccess (Marketplace.MarketplaceStore marketplaceStore)) -> Just marketplaceStore
            _                                                  -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Final marketplaceStore: " <> show marketplaceStore

    _ <- Simulator.callEndpointOnInstance cidInfo "marketplaceFunds" ()
    v <- flip Simulator.waitForState cidInfo $ \json -> case (fromJSON json :: Result (ContractResponse Text Marketplace.InfoContractState)) of
            Success (ContractSuccess (Marketplace.MarketplaceFunds v)) -> Just v
            _                                            -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Final marketplace funds: " <> show v

    _ <- Simulator.callEndpointOnInstance cidInfo "fundsAt" sender
    v <- flip Simulator.waitForState cidInfo $ \json -> case (fromJSON json :: Result (ContractResponse Text Marketplace.InfoContractState)) of
            Success (ContractSuccess (Marketplace.FundsAt v)) -> Just v
            _                                                 -> Nothing
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

handleMarketplaceContract ::
    ( Member (Error PABError) effs
    , Member (LogMsg (PABMultiAgentMsg (Builtin MarketplaceContracts))) effs
    )
    => ContractEffect (Builtin MarketplaceContracts)
    ~> Eff effs
handleMarketplaceContract = Builtin.handleBuiltin getSchema getContract where
  getSchema = \case
    MarketplaceUser _          -> Builtin.endpointsToSchemas @Marketplace.MarketplaceUserSchema
    MarketplaceInfo _          -> Builtin.endpointsToSchemas @Marketplace.MarketplaceInfoSchema
    MarketplaceStart           -> Builtin.endpointsToSchemas @Marketplace.MarketplaceOwnerSchema
  getContract = \case
    MarketplaceInfo marketplace       -> SomeBuiltin $ Marketplace.infoEndpoints marketplace
    MarketplaceUser marketplace       -> SomeBuiltin $ Marketplace.userEndpoints marketplace
    MarketplaceStart           -> SomeBuiltin Marketplace.ownerEndpoints

handlers :: SimulatorEffectHandlers (Builtin MarketplaceContracts)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin MarketplaceContracts) []
    $ interpret handleMarketplaceContract

oneAdaInLovelace :: Integer
oneAdaInLovelace = 1000000
