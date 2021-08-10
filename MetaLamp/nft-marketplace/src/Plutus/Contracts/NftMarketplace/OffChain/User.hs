{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Plutus.Contracts.NftMarketplace.OffChain.User where

import           Control.Lens                                  (_2, _Left,
                                                                _Right, (^.),
                                                                (^?))
import qualified Control.Lens                                  as Lens
import           Control.Monad                                 hiding (fmap)
import qualified Data.Aeson                                    as J
import           Data.Proxy                                    (Proxy (..))
import           Data.Text                                     (Text)
import qualified Data.Text                                     as T
import qualified Ext.Plutus.Contracts.Auction                  as Auction
import           Ext.Plutus.Ledger.Value                       (utxoValue)
import qualified GHC.Generics                                  as Haskell
import           Ledger
import qualified Ledger.Typed.Scripts                          as Scripts
import           Ledger.Typed.Tx
import qualified Ledger.Value                                  as V
import           Plutus.Abstract.ContractResponse
import           Plutus.Contract
import           Plutus.Contract.StateMachine
import           Plutus.Contracts.Currency                     as Currency
import           Plutus.Contracts.NftMarketplace.OffChain.Info (fundsAt,
                                                                mapError',
                                                                marketplaceStore)
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core  as Core
import qualified Plutus.Contracts.Services.Sale                as Sale
import qualified PlutusTx
import qualified PlutusTx.AssocMap                             as AssocMap
import           PlutusTx.Prelude                              hiding
                                                               (Semigroup (..))
import           Prelude                                       (Semigroup (..))
import qualified Prelude                                       as Haskell
import qualified Schema
import           Text.Printf                                   (printf)

getOwnPubKey :: Contract w s Text PubKeyHash
getOwnPubKey = pubKeyHash <$> ownPubKey

data CreateNftParams =
  CreateNftParams {
    cnpIpfsCid        :: ByteString,
    cnpNftName        :: ByteString,
    cnpNftDescription :: ByteString,
    cnpNftCategory    :: Core.Category,
    cnpRevealIssuer   :: Bool
  }
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

PlutusTx.unstableMakeIsData ''CreateNftParams
PlutusTx.makeLift ''CreateNftParams

-- | The user specifies which NFT to mint and add to marketplace store,
--   he gets it into his wallet and the corresponding store entry is created
createNft :: Core.Marketplace -> CreateNftParams -> Contract w s Text ()
createNft marketplace CreateNftParams {..} = do
    let ipfsCidHash = sha2_256 cnpIpfsCid
    nftStore <- Core.mdSingletons <$> marketplaceStore marketplace
    when (isJust $ AssocMap.lookup ipfsCidHash nftStore) $ throwError "Nft entry already exists"

    pkh <- getOwnPubKey
    let tokenName = V.TokenName cnpIpfsCid
    nft <-
           mapError (T.pack . Haskell.show @Currency.CurrencyError) $
           Currency.forgeContract pkh [(tokenName, 1)]

    let client = Core.marketplaceClient marketplace
    let nftEntry = Core.NftInfo
            { niCurrency          = Currency.currencySymbol nft
            , niName        = cnpNftName
            , niDescription = cnpNftDescription
            , niCategory = cnpNftCategory
            , niIssuer      = if cnpRevealIssuer then Just pkh else Nothing
            }
    void $ mapError' $ runStep client $ Core.CreateNftRedeemer ipfsCidHash nftEntry

    logInfo @Haskell.String $ printf "Created NFT %s with store entry %s" (Haskell.show nft) (Haskell.show nftEntry)
    pure ()

data OpenSaleParams =
  OpenSaleParams {
    ospIpfsCid   :: ByteString,
    ospSalePrice :: Sale.LovelacePrice
  }
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

PlutusTx.unstableMakeIsData ''OpenSaleParams
PlutusTx.makeLift ''OpenSaleParams

-- | The user opens sale for his NFT
openSale :: Core.Marketplace -> OpenSaleParams -> Contract w s Text ()
openSale marketplace OpenSaleParams {..} = do
    let ipfsCidHash = sha2_256 ospIpfsCid
    nftStore <- Core.mdSingletons <$> marketplaceStore marketplace
    nftEntry <- maybe (throwError "NFT has not been created") pure $ AssocMap.lookup ipfsCidHash nftStore
    let tokenName = V.TokenName ospIpfsCid

    sale <- Sale.openSale
              Sale.OpenSaleParams {
                  ospSalePrice = ospSalePrice,
                  ospSaleValue = V.singleton (nftEntry ^. Core._nftRecord . Core._niCurrency) tokenName 1
              }

    let client = Core.marketplaceClient marketplace
    let lot = Left sale
    void $ mapError' $ runStep client $ Core.PutLotRedeemer (Left (ipfsCidHash, ospIpfsCid)) lot

    logInfo @Haskell.String $ printf "Created NFT sale %s" (Haskell.show sale)
    pure ()

data BuyNftParams =
  BuyNftParams {
    bnpIpfsCid   :: ByteString
  }
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

PlutusTx.unstableMakeIsData ''BuyNftParams
PlutusTx.makeLift ''BuyNftParams

-- | The user buys specified NFT lot
buyNft :: Core.Marketplace -> BuyNftParams -> Contract w s Text ()
buyNft marketplace BuyNftParams {..} = do
    let ipfsCidHash = sha2_256 bnpIpfsCid
    nftStore <- Core.mdSingletons <$> marketplaceStore marketplace
    nftEntry <- maybe (throwError "NFT has not been created") pure $ AssocMap.lookup ipfsCidHash nftStore
    nftSale <- maybe (throwError "NFT has not been put on sale") pure $
                  nftEntry ^. Core._nftLot ^? traverse . _2 . _Left

    _ <- Sale.buyLot nftSale

    let client = Core.marketplaceClient marketplace
    void $ mapError' $ runStep client $ Core.RemoveLotRedeemer (Left ipfsCidHash)

    logInfo @Haskell.String $ printf "Bought NFT from sale %s" (Haskell.show nftSale)
    pure ()

data CloseSaleParams =
  CloseSaleParams {
    cspIpfsCid   :: ByteString
  }
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

PlutusTx.unstableMakeIsData ''CloseSaleParams
PlutusTx.makeLift ''CloseSaleParams

-- | The user closes NFT sale and receives his token back
closeSale :: Core.Marketplace -> CloseSaleParams -> Contract w s Text ()
closeSale marketplace CloseSaleParams {..} = do
    let ipfsCidHash = sha2_256 cspIpfsCid
    nftStore <- Core.mdSingletons <$> marketplaceStore marketplace
    nftEntry <- maybe (throwError "NFT has not been created") pure $ AssocMap.lookup ipfsCidHash nftStore
    nftSale <- maybe (throwError "NFT has not been put on sale") pure $
                  nftEntry ^. Core._nftLot ^? traverse . _2 . _Left

    _ <- Sale.redeemLot nftSale

    let client = Core.marketplaceClient marketplace
    void $ mapError' $ runStep client $ Core.RemoveLotRedeemer (Left ipfsCidHash)

    logInfo @Haskell.String $ printf "Closed NFT sale %s" (Haskell.show nftSale)
    pure ()

data HoldAnAuctionParams =
  HoldAnAuctionParams {
    haapIpfsCid  :: ByteString,
    haapDuration :: Slot
  }
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

PlutusTx.unstableMakeIsData ''HoldAnAuctionParams
PlutusTx.makeLift ''HoldAnAuctionParams

-- | The user starts an auction for specified NFT
startAnAuction :: Core.Marketplace -> HoldAnAuctionParams -> Contract w s Text ()
startAnAuction marketplace HoldAnAuctionParams {..} = do
    let ipfsCidHash = sha2_256 haapIpfsCid
    nftStore <- Core.mdSingletons <$> marketplaceStore marketplace
    nftEntry <- maybe (throwError "NFT has not been created") pure $ AssocMap.lookup ipfsCidHash nftStore
    let tokenName = V.TokenName haapIpfsCid
    let nftValue = V.singleton (nftEntry ^. Core._nftRecord . Core._niCurrency) tokenName 1

    currSlot <- currentSlot
    let endTime = currSlot + haapDuration
    (auctionToken, auctionParams) <- mapError (T.pack . Haskell.show) $ Auction.startAuction nftValue endTime

    let client = Core.marketplaceClient marketplace
    let lot = Right $ Auction.toTuple auctionToken auctionParams
    void $ mapError' $ runStep client $ Core.PutLotRedeemer (Left (ipfsCidHash, haapIpfsCid)) lot

    logInfo @Haskell.String $ printf "Started an auction %s" (Haskell.show auctionParams)
    pure ()

-- | The user completes the auction for specified NFT
completeAnAuction :: Core.Marketplace -> HoldAnAuctionParams -> Contract w s Text ()
completeAnAuction marketplace HoldAnAuctionParams {..} = do
    let ipfsCidHash = sha2_256 haapIpfsCid
    nftStore <- Core.mdSingletons <$> marketplaceStore marketplace
    nftEntry <- maybe (throwError "NFT has not been created") pure $ AssocMap.lookup ipfsCidHash nftStore
    nftAuction <- maybe (throwError "NFT has not been put on auction") pure $
                  nftEntry ^. Core._nftLot ^? traverse . _2 . _Right

    let auctionToken = Auction.getStateToken nftAuction
    let auctionParams = Auction.fromTuple nftAuction
    _ <- mapError (T.pack . Haskell.show) $ Auction.payoutAuction auctionToken auctionParams

    let client = Core.marketplaceClient marketplace
    void $ mapError' $ runStep client $ Core.RemoveLotRedeemer (Left ipfsCidHash)

    logInfo @Haskell.String $ printf "Completed an auction %s" (Haskell.show nftAuction)
    pure ()

data BidOnAuctionParams =
  BidOnAuctionParams {
    japIpfsCid :: ByteString,
    japBid     :: Ada
  }
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

PlutusTx.unstableMakeIsData ''BidOnAuctionParams
PlutusTx.makeLift ''BidOnAuctionParams

-- | The user submits a bid on the auction for specified NFT
bidOnAuction :: Core.Marketplace -> BidOnAuctionParams -> Contract w s Text ()
bidOnAuction marketplace BidOnAuctionParams {..} = do
    let ipfsCidHash = sha2_256 japIpfsCid
    nftStore <- Core.mdSingletons <$> marketplaceStore marketplace
    nftEntry <- maybe (throwError "NFT has not been created") pure $ AssocMap.lookup ipfsCidHash nftStore
    nftAuction <- maybe (throwError "NFT has not been put on auction") pure $
                  nftEntry ^. Core._nftLot ^? traverse . _2 . _Right

    let auctionToken = Auction.getStateToken nftAuction
    let auctionParams = Auction.fromTuple nftAuction
    _ <- mapError (T.pack . Haskell.show) $ Auction.submitBid auctionToken auctionParams japBid

    logInfo @Haskell.String $ printf "Submitted bid for NFT auction %s" (Haskell.show nftAuction)
    pure ()

data BundleUpParams =
  BundleUpParams {
    bupIpfsCids    :: [ByteString],
    bupName        :: ByteString,
    bupDescription :: ByteString,
    bupCategory    :: Core.Category
  }
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

PlutusTx.unstableMakeIsData ''BundleUpParams
PlutusTx.makeLift ''BundleUpParams

-- | The user creates a bundle from specified NFTs
bundleUp :: Core.Marketplace -> BundleUpParams -> Contract w s Text ()
bundleUp marketplace BundleUpParams {..} = do
    let bundleId = Core.calcBundleIdHash bupIpfsCids
    let nftIds = sha2_256 <$> bupIpfsCids
    let bundleInfo = Core.BundleInfo
          { biName        = bupName
          , biDescription = bupDescription
          , biCategory    = bupCategory
          }

    let client = Core.marketplaceClient marketplace
    void $ mapError' $ runStep client $ Core.BundleUpRedeemer nftIds bundleId bundleInfo

    logInfo @Haskell.String $ printf "Created a bundle %s" (Haskell.show bundleInfo)
    pure ()

data UnbundleParams =
  UnbundleParams {
    upIpfsCids :: [ByteString]
  }
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

PlutusTx.unstableMakeIsData ''UnbundleParams
PlutusTx.makeLift ''UnbundleParams

-- | The user unbundles specified NFTs
unbundle :: Core.Marketplace -> UnbundleParams -> Contract w s Text ()
unbundle marketplace UnbundleParams {..} = do
    let bundleId = Core.calcBundleIdHash upIpfsCids

    let client = Core.marketplaceClient marketplace
    void $ mapError' $ runStep client $ Core.UnbundleRedeemer bundleId

    logInfo @Haskell.String $ printf "Removed bundle by id %s" (Haskell.show bundleId)
    pure ()

balanceAt :: PubKeyHash -> AssetClass -> Contract w s Text Integer
balanceAt pkh asset = flip V.assetClassValueOf asset <$> fundsAt pkh

ownPubKeyBalance :: Contract w s Text Value
ownPubKeyBalance = getOwnPubKey >>= fundsAt

type MarketplaceUserSchema =
    Endpoint "createNft" CreateNftParams
    .\/ Endpoint "openSale" OpenSaleParams
    .\/ Endpoint "buyNft" BuyNftParams
    .\/ Endpoint "closeSale" CloseSaleParams
    .\/ Endpoint "startAnAuction" HoldAnAuctionParams
    .\/ Endpoint "completeAnAuction" HoldAnAuctionParams
    .\/ Endpoint "bidOnAuction" BidOnAuctionParams
    .\/ Endpoint "bundleUp" BundleUpParams
    .\/ Endpoint "unbundle" UnbundleParams
    .\/ Endpoint "ownPubKey" ()
    .\/ Endpoint "ownPubKeyBalance" ()

data UserContractState =
    NftCreated
    | OpenedSale
    | NftBought
    | ClosedSale
    | AuctionStarted
    | AuctionComplete
    | BidSubmitted
    | Bundled
    | Unbundled
    | GetPubKey PubKeyHash
    | GetPubKeyBalance Value
    deriving stock (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON)

Lens.makeClassyPrisms ''UserContractState

userEndpoints :: Core.Marketplace -> Contract (ContractResponse Text UserContractState) MarketplaceUserSchema Void ()
userEndpoints marketplace = forever $
    withContractResponse (Proxy @"createNft") (const NftCreated) (createNft marketplace)
    `select` withContractResponse (Proxy @"openSale") (const OpenedSale) (openSale marketplace)
    `select` withContractResponse (Proxy @"buyNft") (const NftBought) (buyNft marketplace)
    `select` withContractResponse (Proxy @"closeSale") (const ClosedSale) (closeSale marketplace)
    `select` withContractResponse (Proxy @"startAnAuction") (const AuctionStarted) (startAnAuction marketplace)
    `select` withContractResponse (Proxy @"completeAnAuction") (const AuctionComplete) (completeAnAuction marketplace)
    `select` withContractResponse (Proxy @"bidOnAuction") (const BidSubmitted) (bidOnAuction marketplace)
    `select` withContractResponse (Proxy @"bundleUp") (const Bundled) (bundleUp marketplace)
    `select` withContractResponse (Proxy @"unbundle") (const Unbundled) (unbundle marketplace)
    `select` withContractResponse (Proxy @"ownPubKey") GetPubKey (const getOwnPubKey)
    `select` withContractResponse (Proxy @"ownPubKeyBalance") GetPubKeyBalance (const ownPubKeyBalance)
