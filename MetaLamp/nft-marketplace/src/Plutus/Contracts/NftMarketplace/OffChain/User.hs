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
import           Plutus.Contracts.NftMarketplace.OffChain.ID
import           Plutus.Contracts.NftMarketplace.OffChain.Info
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
Lens.makeClassy_ ''CreateNftParams

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
    ospItemId    :: UserItemId,
    ospSalePrice :: Sale.LovelacePrice
  }
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

PlutusTx.unstableMakeIsData ''OpenSaleParams
PlutusTx.makeLift ''OpenSaleParams
Lens.makeClassy_ ''OpenSaleParams

-- | The user opens sale for his NFT
openSale :: Core.Marketplace -> OpenSaleParams -> Contract w s Text ()
openSale marketplace OpenSaleParams {..} = do
    let internalId = toInternalId ospItemId
    nftStore <- marketplaceStore marketplace
    saleValue <- case internalId of
      Left nftId@(Core.InternalNftId ipfsCidHash ipfsCid) ->
        Core.nftValue ipfsCid <$> getNftEntry nftStore nftId
      Right bundleId@(Core.InternalBundleId bundleHash cids) ->
        Core.bundleValue cids <$> getBundleEntry nftStore bundleId

    sale <- Sale.openSale
              Sale.OpenSaleParams {
                  ospSalePrice = ospSalePrice,
                  ospSaleValue = saleValue
              }

    let client = Core.marketplaceClient marketplace
    let lot = Left sale
    void $ mapError' $ runStep client $ Core.PutLotRedeemer internalId lot

    logInfo @Haskell.String $ printf "Created NFT sale %s" (Haskell.show sale)
    pure ()

data CompleteSaleParams =
  CompleteSaleParams {
    cspItemId    :: UserItemId
  }
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

PlutusTx.unstableMakeIsData ''CompleteSaleParams
PlutusTx.makeLift ''CompleteSaleParams
Lens.makeClassy_ ''CompleteSaleParams

-- | The user buys specified NFT lot
buyItem :: Core.Marketplace -> CompleteSaleParams -> Contract w s Text ()
buyItem marketplace CompleteSaleParams {..} = do
    let internalId = toInternalId cspItemId
    nftStore <- marketplaceStore marketplace
    sale <- case internalId of
      Left nftId@(Core.InternalNftId ipfsCidHash ipfsCid) -> do
        nftEntry <- getNftEntry nftStore nftId
        maybe (throwError "NFT has not been put on sale") pure $
            nftEntry ^. Core._nftLot ^? traverse . _2 . _Left
      Right bundleId@(Core.InternalBundleId bundleHash cids) -> do
        bundleEntry <- getBundleEntry nftStore bundleId
        maybe (throwError "Bundle has not been put on sale") pure $
            bundleEntry ^. Core._nbTokens ^? Core._HasLot . _2 . _Left

    _ <- Sale.buyLot sale

    let client = Core.marketplaceClient marketplace
    void $ mapError' $ runStep client $ Core.RemoveLotRedeemer $
      Lens.bimap Core.iniIpfsCidHash Core.ibiBundleId internalId

    logInfo @Haskell.String $ printf "Bought lot from sale %s" (Haskell.show sale)
    pure ()

-- | The user closes NFT sale and receives his token back
closeSale :: Core.Marketplace -> CompleteSaleParams -> Contract w s Text ()
closeSale marketplace CompleteSaleParams {..} = do
    let internalId = toInternalId cspItemId
    nftStore <- marketplaceStore marketplace
    sale <- case internalId of
      Left nftId@(Core.InternalNftId ipfsCidHash ipfsCid) -> do
        nftEntry <- getNftEntry nftStore nftId
        maybe (throwError "NFT has not been put on sale") pure $
            nftEntry ^. Core._nftLot ^? traverse . _2 . _Left
      Right bundleId@(Core.InternalBundleId bundleHash cids) -> do
        bundleEntry <- getBundleEntry nftStore bundleId
        maybe (throwError "Bundle has not been put on sale") pure $
            bundleEntry ^. Core._nbTokens ^? Core._HasLot . _2 . _Left

    _ <- Sale.redeemLot sale

    let client = Core.marketplaceClient marketplace
    void $ mapError' $ runStep client $ Core.RemoveLotRedeemer $
      Lens.bimap Core.iniIpfsCidHash Core.ibiBundleId internalId

    logInfo @Haskell.String $ printf "Closed lot sale %s" (Haskell.show sale)
    pure ()

data HoldAnAuctionParams =
  HoldAnAuctionParams {
    haapItemId   :: UserItemId,
    haapDuration :: Slot
  }
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

PlutusTx.unstableMakeIsData ''HoldAnAuctionParams
PlutusTx.makeLift ''HoldAnAuctionParams
Lens.makeClassy_ ''HoldAnAuctionParams

-- | The user starts an auction for specified NFT
startAnAuction :: Core.Marketplace -> HoldAnAuctionParams -> Contract w s Text ()
startAnAuction marketplace HoldAnAuctionParams {..} = do
    let internalId = toInternalId haapItemId
    nftStore <- marketplaceStore marketplace
    auctionValue <- case internalId of
      Left nftId@(Core.InternalNftId ipfsCidHash ipfsCid) ->
        Core.nftValue ipfsCid <$> getNftEntry nftStore nftId
      Right bundleId@(Core.InternalBundleId bundleHash cids) ->
        Core.bundleValue cids <$> getBundleEntry nftStore bundleId

    currSlot <- currentSlot
    let endTime = currSlot + haapDuration
    (auctionToken, auctionParams) <- mapError (T.pack . Haskell.show) $ Auction.startAuction auctionValue endTime

    let client = Core.marketplaceClient marketplace
    let lot = Right $ Auction.toTuple auctionToken auctionParams
    void $ mapError' $ runStep client $ Core.PutLotRedeemer internalId lot

    logInfo @Haskell.String $ printf "Started an auction %s" (Haskell.show auctionParams)
    pure ()

-- | The user completes the auction for specified NFT
completeAnAuction :: Core.Marketplace -> HoldAnAuctionParams -> Contract w s Text ()
completeAnAuction marketplace HoldAnAuctionParams {..} = do
    let internalId = toInternalId haapItemId
    nftStore <- marketplaceStore marketplace
    auction <- case internalId of
      Left nftId@(Core.InternalNftId ipfsCidHash ipfsCid) -> do
        nftEntry <- getNftEntry nftStore nftId
        maybe (throwError "NFT has not been put on auction") pure $
            nftEntry ^. Core._nftLot ^? traverse . _2 . _Right
      Right bundleId@(Core.InternalBundleId bundleHash cids) -> do
        bundleEntry <- getBundleEntry nftStore bundleId
        maybe (throwError "Bundle has not been put on auction") pure $
            bundleEntry ^. Core._nbTokens ^? Core._HasLot . _2 . _Right

    let auctionToken = Auction.getStateToken auction
    let auctionParams = Auction.fromTuple auction
    _ <- mapError (T.pack . Haskell.show) $ Auction.payoutAuction auctionToken auctionParams

    let client = Core.marketplaceClient marketplace
    void $ mapError' $ runStep client $ Core.RemoveLotRedeemer $
      Lens.bimap Core.iniIpfsCidHash Core.ibiBundleId internalId

    logInfo @Haskell.String $ printf "Completed an auction %s" (Haskell.show auction)
    pure ()

data BidOnAuctionParams =
  BidOnAuctionParams {
    boapItemId :: UserItemId,
    boapBid    :: Ada
  }
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

PlutusTx.unstableMakeIsData ''BidOnAuctionParams
PlutusTx.makeLift ''BidOnAuctionParams
Lens.makeClassy_ ''BidOnAuctionParams

-- | The user submits a bid on the auction for specified NFT
bidOnAuction :: Core.Marketplace -> BidOnAuctionParams -> Contract w s Text ()
bidOnAuction marketplace BidOnAuctionParams {..} = do
    let internalId = toInternalId boapItemId
    nftStore <- marketplaceStore marketplace
    auction <- case internalId of
      Left nftId@(Core.InternalNftId ipfsCidHash ipfsCid) -> do
        nftEntry <- getNftEntry nftStore nftId
        maybe (throwError "NFT has not been put on auction") pure $
            nftEntry ^. Core._nftLot ^? traverse . _2 . _Right
      Right bundleId@(Core.InternalBundleId bundleHash cids) -> do
        bundleEntry <- getBundleEntry nftStore bundleId
        maybe (throwError "Bundle has not been put on auction") pure $
            bundleEntry ^. Core._nbTokens ^? Core._HasLot . _2 . _Right

    let auctionToken = Auction.getStateToken auction
    let auctionParams = Auction.fromTuple auction
    _ <- mapError (T.pack . Haskell.show) $ Auction.submitBid auctionToken auctionParams boapBid

    logInfo @Haskell.String $ printf "Submitted bid for auction %s" (Haskell.show auction)
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
Lens.makeClassy_ ''BundleUpParams

-- | The user creates a bundle from specified NFTs
bundleUp :: forall w s. Core.Marketplace -> BundleUpParams -> Contract w s Text ()
bundleUp marketplace BundleUpParams {..} = do
    let bundleId = Core.calcBundleIdHash bupIpfsCids
    bundles <- Core.mdBundles <$> marketplaceStore marketplace
    when (isJust $ AssocMap.lookup bundleId bundles) $ throwError "Bundle entry already exists"
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
Lens.makeClassy_ ''UnbundleParams

-- | The user unbundles specified NFTs
unbundle :: Core.Marketplace -> UnbundleParams -> Contract w s Text ()
unbundle marketplace UnbundleParams {..} = do
    let bundleId = Core.calcBundleIdHash upIpfsCids
    bundles <- Core.mdBundles <$> marketplaceStore marketplace
    when (isNothing $ AssocMap.lookup bundleId bundles) $ throwError "Bundle entry does not exist"

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
    .\/ Endpoint "buyItem" CompleteSaleParams
    .\/ Endpoint "closeSale" CompleteSaleParams
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
    `select` withContractResponse (Proxy @"buyItem") (const NftBought) (buyItem marketplace)
    `select` withContractResponse (Proxy @"closeSale") (const ClosedSale) (closeSale marketplace)
    `select` withContractResponse (Proxy @"startAnAuction") (const AuctionStarted) (startAnAuction marketplace)
    `select` withContractResponse (Proxy @"completeAnAuction") (const AuctionComplete) (completeAnAuction marketplace)
    `select` withContractResponse (Proxy @"bidOnAuction") (const BidSubmitted) (bidOnAuction marketplace)
    `select` withContractResponse (Proxy @"bundleUp") (const Bundled) (bundleUp marketplace)
    `select` withContractResponse (Proxy @"unbundle") (const Unbundled) (unbundle marketplace)
    `select` withContractResponse (Proxy @"ownPubKey") GetPubKey (const getOwnPubKey)
    `select` withContractResponse (Proxy @"ownPubKeyBalance") GetPubKeyBalance (const ownPubKeyBalance)
