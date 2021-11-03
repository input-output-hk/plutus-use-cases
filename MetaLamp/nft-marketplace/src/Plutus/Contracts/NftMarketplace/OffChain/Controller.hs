{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RecordWildCards         #-}

module Plutus.Contracts.NftMarketplace.OffChain.Controller where

import           Control.Lens                                 (_2, _Left,
                                                               _Right, (^.),
                                                               (^?))
import qualified Control.Lens                                 as Lens
import           Control.Monad                                hiding (fmap)
import qualified Data.Aeson                                   as J
import           Data.Proxy                                   (Proxy (..))
import           Data.Text                                    (Text)
import qualified Data.Text                                    as T
import qualified Plutus.Contracts.Services.Auction                        as Auction
import           Ext.Plutus.Ledger.Value                      (ChainIndexTxMap,
                                                               utxosValue)
import qualified GHC.Generics                                 as Haskell
import           Ledger
import qualified Ledger.Typed.Scripts                         as Scripts
import           Ledger.Typed.Tx
import           Ledger.Value
import           Plutus.Abstract.ContractResponse             (ContractResponse, withContractResponse)
import           Plutus.Abstract.RemoteData                   (RemoteData)
import           Plutus.Contract
import           Plutus.Contract.StateMachine
import           Plutus.Contracts.Currency                    as Currency
import           Plutus.Contracts.NftMarketplace.OffChain.ID
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Core
import qualified PlutusTx
import qualified PlutusTx.AssocMap                            as AssocMap
import           PlutusTx.Prelude                             hiding
                                                              (Semigroup (..))
import           Prelude                                      (Semigroup (..))
import qualified Prelude                                      as Haskell
import           Text.Printf                                  (printf)
import Plutus.Contracts.NftMarketplace.OffChain.Info (marketplaceStore)
import Plutus.Contracts.Services.Sale.Endpoints (mapError')

collect :: Core.Marketplace -> Contract w s Text ()
collect marketplace = do
  Core.MarketplaceDatum {..} <- marketplaceStore marketplace
  currTime <- currentTime
  let singletons = mapMaybe (fromSingleton currTime) . AssocMap.toList $ mdSingletons
  let bundles = mapMaybe (fromBundle currTime) . AssocMap.toList $ mdBundles

  void $ traverse 
    (\(redeemer, auction) -> completeAnAuction' marketplace redeemer auction) 
    (singletons <> bundles)
    where
      fromSingleton :: POSIXTime -> (Core.IpfsCidHash, Core.NFT) -> Maybe (Core.MarketplaceRedeemer, Auction.Auction)
      fromSingleton currTime (hash, nft) = do
        auction <- Core.getAuctionFromNFT nft
        guard (currTime >= (Auction.aEndTime auction))
        let redeemer = Core.RemoveLotRedeemer . Core.RemoveNftLotRedeemer $ hash
        pure (redeemer, auction)
      fromBundle :: POSIXTime -> (Core.BundleId, Core.NftBundle) -> Maybe (Core.MarketplaceRedeemer, Auction.Auction)
      fromBundle currTime (bundleId, bundle) = do
        auction <- Core.getAuctionFromBundle bundle
        guard (currTime >= (Auction.aEndTime auction)) 
        let redeemer = Core.RemoveLotRedeemer . Core.RemoveBundleLotRedeemer $ bundleId
        pure (redeemer, auction)

completeAnAuction' :: Core.Marketplace -> Core.MarketplaceRedeemer -> Auction.Auction -> Contract w s Text ()
completeAnAuction' marketplace redeemer auction = do
  _ <- mapError (T.pack . Haskell.show) $ Auction.payoutAuction auction

  let client = Core.marketplaceClient marketplace
  void $ mapError' $ runStep client redeemer

  logInfo @Haskell.String $ printf "Completed an auction %s" (Haskell.show auction)
  pure ()

type MarketplaceControllerSchema =
    Endpoint "collect" ()

data ControllerContractState =
    Collected ()
    deriving stock (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON)

Lens.makeClassyPrisms ''ControllerContractState

controllerEndpoints :: Core.Marketplace -> Promise (ContractResponse Haskell.String Text ControllerContractState) MarketplaceControllerSchema Void ()
controllerEndpoints marketplace =
    withContractResponse (Proxy @"collect") Collected (const $ collect marketplace) 
      <> controllerEndpoints marketplace
