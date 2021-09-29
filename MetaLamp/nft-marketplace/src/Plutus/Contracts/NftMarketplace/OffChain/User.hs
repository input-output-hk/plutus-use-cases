{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Plutus.Contracts.NftMarketplace.OffChain.User where

import           Control.Lens                                           (_2,
                                                                         _Left,
                                                                         _Right,
                                                                         (^.),
                                                                         (^?))
import qualified Control.Lens                                           as Lens
import           Control.Monad                                          hiding
                                                                        (fmap)
import qualified Data.Aeson                                             as J
import           Data.Proxy                                             (Proxy (..))
import           Data.Text                                              (Text)
import qualified Data.Text                                              as T
import qualified Ext.Plutus.Contracts.Auction                           as Auction
import           Ext.Plutus.Ledger.Value                                (utxoValue)
import qualified GHC.Generics                                           as Haskell
import           Ledger
import qualified Ledger.Typed.Scripts                                   as Scripts
import           Ledger.Typed.Tx
import qualified Ledger.Value                                           as V
import           Plutus.Abstract.ContractResponse
import           Plutus.Contract
import           Plutus.Contract.StateMachine
import           Plutus.Contracts.Currency                              as Currency
import           Plutus.Contracts.NftMarketplace.OffChain.ID
import           Plutus.Contracts.NftMarketplace.OffChain.Info
import           Plutus.Contracts.NftMarketplace.OffChain.Serialization (deserializeByteString)
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core           as Core
import qualified Plutus.Contracts.Services.Sale                         as Sale
import qualified PlutusTx
import qualified PlutusTx.AssocMap                                      as AssocMap
import           PlutusTx.Prelude                                       hiding
                                                                        (Semigroup (..))
import           Prelude                                                (Semigroup (..))
import qualified Prelude                                                as Haskell
import qualified Schema
import           Text.Printf                                            (printf)
import qualified Plutus.Abstract.ContractResp as S

getOwnPubKey :: Contract w s Text PubKeyHash
getOwnPubKey = pubKeyHash <$> ownPubKey

data CreateNftParams =
  CreateNftParams {
    cnpIpfsCid        :: Text,
    cnpNftName        :: Text,
    cnpNftDescription :: Text,
    cnpNftCategory    :: [Text],
    cnpRevealIssuer   :: Bool
  }
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

Lens.makeClassy_ ''CreateNftParams

-- | The user specifies which NFT to mint and add to marketplace store,
--   he gets it into his wallet and the corresponding store entry is created
createNft :: Core.Marketplace -> CreateNftParams -> Contract w s Text ()
createNft marketplace CreateNftParams {..} = do
    let ipfsCid = deserializeByteString cnpIpfsCid
    let ipfsCidHash = sha2_256 ipfsCid
    nftStore <- Core.mdSingletons <$> marketplaceStore marketplace
    when (isJust $ AssocMap.lookup ipfsCidHash nftStore) $ throwError "Nft entry already exists"

    pkh <- getOwnPubKey
    let tokenName = V.TokenName ipfsCid
    nft <-
           mapError (T.pack . Haskell.show @Currency.CurrencyError) $
           Currency.mintContract pkh [(tokenName, 1)]

    let client = Core.marketplaceClient marketplace
    let nftEntry = Core.NftInfo
            { niCurrency          = Currency.currencySymbol nft
            , niName        = deserializeByteString cnpNftName
            , niDescription = deserializeByteString cnpNftDescription
            , niCategory = deserializeByteString <$> cnpNftCategory
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

data CloseLotParams =
  CloseLotParams {
    clpItemId    :: UserItemId
  }
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

Lens.makeClassy_ ''CloseLotParams

-- | The user buys specified NFT lot
buyItem :: Core.Marketplace -> CloseLotParams -> Contract w s Text ()
buyItem marketplace CloseLotParams {..} = do
    let internalId = toInternalId clpItemId
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
closeSale :: Core.Marketplace -> CloseLotParams -> Contract w s Text ()
closeSale marketplace CloseLotParams {..} = do
    let internalId = toInternalId clpItemId
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

deriving newtype instance Schema.ToSchema DiffMilliSeconds

data StartAnAuctionParams =
  StartAnAuctionParams {
    saapItemId   :: UserItemId,
    saapDuration :: DiffMilliSeconds
  }
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

Lens.makeClassy_ ''StartAnAuctionParams

-- | The user starts an auction for specified NFT
startAnAuction :: Core.Marketplace -> StartAnAuctionParams -> Contract w s Text ()
startAnAuction marketplace StartAnAuctionParams {..} = do
    let internalId = toInternalId saapItemId
    nftStore <- marketplaceStore marketplace
    auctionValue <- case internalId of
      Left nftId@(Core.InternalNftId ipfsCidHash ipfsCid) ->
        Core.nftValue ipfsCid <$> getNftEntry nftStore nftId
      Right bundleId@(Core.InternalBundleId bundleHash cids) ->
        Core.bundleValue cids <$> getBundleEntry nftStore bundleId

    currTime <- currentTime
    let endTime = currTime + fromMilliSeconds saapDuration
    (auctionToken, auctionParams) <- mapError (T.pack . Haskell.show) $ Auction.startAuction auctionValue endTime

    let client = Core.marketplaceClient marketplace
    let lot = Right $ Auction.toTuple auctionToken auctionParams
    void $ mapError' $ runStep client $ Core.PutLotRedeemer internalId lot

    logInfo @Haskell.String $ printf "Started an auction %s" (Haskell.show auctionParams)
    pure ()

-- | The user completes the auction for specified NFT
completeAnAuction :: Core.Marketplace -> CloseLotParams -> Contract w s Text ()
completeAnAuction marketplace CloseLotParams {..} = do
    let internalId = toInternalId clpItemId
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
    bupIpfsCids    :: [Text],
    bupName        :: Text,
    bupDescription :: Text,
    bupCategory    :: [Text]
  }
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

Lens.makeClassy_ ''BundleUpParams

-- | The user creates a bundle from specified NFTs
bundleUp :: forall w s. Core.Marketplace -> BundleUpParams -> Contract w s Text ()
bundleUp marketplace BundleUpParams {..} = do
    let ipfsCids = deserializeByteString <$> bupIpfsCids
    let bundleId = Core.calcBundleIdHash ipfsCids
    bundles <- Core.mdBundles <$> marketplaceStore marketplace
    when (isJust $ AssocMap.lookup bundleId bundles) $ throwError "Bundle entry already exists"
    let nftIds = sha2_256 <$> ipfsCids
    let bundleInfo = Core.BundleInfo
          { biName        = deserializeByteString bupName
          , biDescription = deserializeByteString bupDescription
          , biCategory    = deserializeByteString <$> bupCategory
          }

    let client = Core.marketplaceClient marketplace
    void $ mapError' $ runStep client $ Core.BundleUpRedeemer nftIds bundleId bundleInfo

    logInfo @Haskell.String $ printf "Created a bundle %s" (Haskell.show bundleInfo)
    pure ()

data UnbundleParams =
  UnbundleParams {
    upIpfsCids :: [Text]
  }
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

Lens.makeClassy_ ''UnbundleParams

-- | The user unbundles specified NFTs
unbundle :: Core.Marketplace -> UnbundleParams -> Contract w s Text ()
unbundle marketplace UnbundleParams {..} = do
    let bundleId = Core.calcBundleIdHash $ fmap deserializeByteString upIpfsCids
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

type MarketplaceUserSchemaR =
    S.EndpointReturning "createNft" CreateNftParams ()
    .\/ S.EndpointReturning "openSale" OpenSaleParams ()
    .\/ S.EndpointReturning "buyItem" CloseLotParams ()
    .\/ S.EndpointReturning "closeSale" CloseLotParams ()
    .\/ S.EndpointReturning "startAnAuction" StartAnAuctionParams ()
    .\/ S.EndpointReturning "completeAnAuction" CloseLotParams ()
    .\/ S.EndpointReturning "bidOnAuction" BidOnAuctionParams ()
    .\/ S.EndpointReturning "bundleUp" BundleUpParams ()
    .\/ S.EndpointReturning "unbundle" UnbundleParams ()
    .\/ S.EndpointReturning "ownPubKey" () ()
    .\/ S.EndpointReturning "ownPubKeyBalance" () ()

type MarketplaceUserSchema = S.GetEndpoints MarketplaceUserSchemaR

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

userEndpoints :: Core.Marketplace -> Promise (ContractResponse Text UserContractState) MarketplaceUserSchema Void ()
userEndpoints marketplace =
    (withContractResponse (Proxy @"createNft") (const NftCreated) (createNft marketplace)
    `select` withContractResponse (Proxy @"openSale") (const OpenedSale) (openSale marketplace)
    `select` withContractResponse (Proxy @"buyItem") (const NftBought) (buyItem marketplace)
    `select` withContractResponse (Proxy @"closeSale") (const ClosedSale) (closeSale marketplace)
    `select` withContractResponse (Proxy @"startAnAuction") (const AuctionStarted) (startAnAuction marketplace)
    `select` withContractResponse (Proxy @"completeAnAuction") (const AuctionComplete) (completeAnAuction marketplace)
    `select` withContractResponse (Proxy @"bidOnAuction") (const BidSubmitted) (bidOnAuction marketplace)
    `select` withContractResponse (Proxy @"bundleUp") (const Bundled) (bundleUp marketplace)
    `select` withContractResponse (Proxy @"unbundle") (const Unbundled) (unbundle marketplace)
    `select` withContractResponse (Proxy @"ownPubKey") GetPubKey (const getOwnPubKey)
    `select` withContractResponse (Proxy @"ownPubKeyBalance") GetPubKeyBalance (const ownPubKeyBalance)) <> userEndpoints marketplace
