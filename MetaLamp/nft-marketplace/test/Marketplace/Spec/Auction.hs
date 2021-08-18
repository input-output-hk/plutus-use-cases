{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}

module Marketplace.Spec.Auction
  ( tests
  ) where

import           Control.Lens                                 (_2, _Left,
                                                               _Right, (&),
                                                               (.~), (^.), (^?))
import qualified Control.Lens                                 as Lens
import           Control.Monad                                (void)
import           Data.Foldable                                (find)
import           Data.Function                                (on)
import qualified Data.Map                                     as Map
import           Data.Maybe                                   (isNothing)
import           Data.Text                                    (Text)
import           Data.Void
import qualified Ext.Plutus.Contracts.Auction                 as Auction
import           Ledger
import qualified Ledger.Ada                                   as Ada
import qualified Ledger.Value                                 as V
import qualified Marketplace.Fixtures                         as Fixtures
import qualified Marketplace.Spec.Bundles                     as Bundles
import qualified Marketplace.Spec.CreateNft                   as CreateNft
import qualified Marketplace.Spec.Start                       as Start
import           Plutus.Abstract.ContractResponse
import           Plutus.Contract
import           Plutus.Contract.Test
import qualified Plutus.Contracts.NftMarketplace.Endpoints    as Marketplace
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Marketplace
import qualified Plutus.Trace                                 as Trace
import qualified PlutusTx.AssocMap                            as AssocMap
import           PlutusTx.Builtins                            (sha2_256)
import           PlutusTx.Prelude                             (ByteString)
import           Test.Tasty
import qualified Utils
import           Wallet.Emulator.Wallet

tests :: TestTree
tests =
  testGroup
    "Auction"
  [testGroup
    "NFT singletons"
    [
      checkPredicateOptions
        Fixtures.options
        "Should open an auction for one NFT locking it in auction script & saving link"
        (startAnAuctionValueCheck .&&. startAnAuctionDatumsCheck)
        (void startAnAuctionTrace),
      checkPredicateOptions
        Fixtures.options
        "Should not put on auction if NFT does not exist"
        errorCheckUser
        startAnAuctionTrace',
      checkPredicateOptions
        Fixtures.options
        "Should close auction and pay locked NFT back if there were no bids"
        (completeAnAuctionValueCheck .&&. completeAuctionDatumsCheck)
        completeAnAuctionTrace,
      checkPredicateOptions
        Fixtures.options
        "Should not close auction if it was not started"
        errorCheckUser
        completeAnAuctionTrace',
      checkPredicateOptions
        Fixtures.options
        "Should bid on NFT"
        startAnAuctionDatumsCheck
        (void bidOnAuctionTrace),
      checkPredicateOptions
        Fixtures.options
        "Should not bid if NFT is not on auction"
        errorCheckBuyer
        bidOnAuctionTrace',
      checkPredicateOptions
        Fixtures.options
        "Should close auction and pay locked NFT to the highest bidder"
        (buyOnAuctionValueCheck .&&. completeAuctionDatumsCheck)
        buyOnAuctionTrace
    ],
  testGroup
    "NFT bundles"
    [
      checkPredicateOptions
        Fixtures.options
        "Should open an auction for NFT bundle locking its value in auction script & saving link"
        (startAnAuctionValueCheckB .&&. startAnAuctionDatumsCheckB)
        (void startAnAuctionTraceB),
      checkPredicateOptions
        Fixtures.options
        "Should not put on auction if bundle does not exist"
        errorCheckUser
        startAnAuctionTraceB',
      checkPredicateOptions
        Fixtures.options
        "Should close auction and pay locked bundle value back if there were no bids"
        (completeAnAuctionValueCheckB .&&. completeAuctionDatumsCheckB)
        completeAnAuctionTraceB,
      checkPredicateOptions
        Fixtures.options
        "Should bid on bundle"
        startAnAuctionDatumsCheckB
        (void bidOnAuctionTraceB),
      checkPredicateOptions
        Fixtures.options
        "Should close auction and pay locked bundle value to the highest bidder"
        (buyOnAuctionValueCheckB .&&. completeAuctionDatumsCheckB)
        buyOnAuctionTraceB
    ]]

auctionValue :: Marketplace.Auction -> Value
auctionValue = Auction.apAsset . Auction.fromTuple

-- \/\/\/ "NFT singletons"
startAnAuctionParams ::        Marketplace.StartAnAuctionParams
startAnAuctionParams =  Marketplace.StartAnAuctionParams
        {
    saapItemId   = Marketplace.UserNftId Fixtures.catTokenIpfsCid,
    saapDuration = 155
  }

closeLotParams ::        Marketplace.CloseLotParams
closeLotParams =  Marketplace.CloseLotParams {
                      clpItemId    = Marketplace.UserNftId Fixtures.catTokenIpfsCid
                    }
bidOnAuctionParams :: Marketplace.BidOnAuctionParams
bidOnAuctionParams = Marketplace.BidOnAuctionParams {
            boapItemId = Marketplace.UserNftId Fixtures.catTokenIpfsCid,
            boapBid    = fromInteger $ 25 * Fixtures.oneAdaInLovelace
          }

startAnAuctionTrace :: Trace.EmulatorTrace (Trace.ContractHandle (ContractResponse Text Marketplace.UserContractState) Marketplace.MarketplaceUserSchema Void)
startAnAuctionTrace = do
  h <- CreateNft.createNftTrace

  _ <- Trace.callEndpoint @"startAnAuction" h startAnAuctionParams

  _ <- Trace.waitNSlots 50
  pure h

startAnAuctionTrace' :: Trace.EmulatorTrace ()
startAnAuctionTrace' = do
  _ <- Start.startTrace
  h <- Trace.activateContractWallet Fixtures.userWallet $ Marketplace.userEndpoints Fixtures.marketplace

  _ <- Trace.callEndpoint @"startAnAuction" h startAnAuctionParams

  _ <- Trace.waitNSlots 50
  pure ()

completeAnAuctionTrace :: Trace.EmulatorTrace ()
completeAnAuctionTrace = do
  h <- startAnAuctionTrace

  _ <- Trace.callEndpoint @"completeAnAuction" h closeLotParams

  _ <- Trace.waitNSlots 250
  pure ()

completeAnAuctionTrace' :: Trace.EmulatorTrace ()
completeAnAuctionTrace' = do
  h <- CreateNft.createNftTrace

  _ <- Trace.callEndpoint @"completeAnAuction" h closeLotParams

  _ <- Trace.waitNSlots 50
  pure ()

bidOnAuctionTrace :: Trace.EmulatorTrace (Trace.ContractHandle (ContractResponse Text Marketplace.UserContractState) Marketplace.MarketplaceUserSchema Void)
bidOnAuctionTrace = do
  _ <- startAnAuctionTrace

  h <- Trace.activateContractWallet Fixtures.buyerWallet $ Marketplace.userEndpoints Fixtures.marketplace
  _ <- Trace.callEndpoint @"bidOnAuction" h bidOnAuctionParams

  _ <- Trace.waitNSlots 50
  pure h

bidOnAuctionTrace' :: Trace.EmulatorTrace ()
bidOnAuctionTrace' = do
  _ <- CreateNft.createNftTrace

  h <- Trace.activateContractWallet Fixtures.buyerWallet $ Marketplace.userEndpoints Fixtures.marketplace
  _ <- Trace.callEndpoint @"bidOnAuction" h bidOnAuctionParams

  _ <- Trace.waitNSlots 50
  pure ()

buyOnAuctionTrace :: Trace.EmulatorTrace ()
buyOnAuctionTrace = do
  h <- bidOnAuctionTrace

  _ <- Trace.callEndpoint @"completeAnAuction" h closeLotParams

  _ <- Trace.waitNSlots 250
  pure ()

startAnAuctionDatumsCheck :: TracePredicate
startAnAuctionDatumsCheck =
  dataAtAddress
    Fixtures.marketplaceAddress
    (nftIsOnAuction . Marketplace.mdSingletons)
    where
      nftIsOnAuction = maybe False (\t -> t ^. Marketplace._nftLot ^? traverse . _2 . _Right & fmap auctionValue &
                                (== Just (Marketplace.nftValue Fixtures.catTokenIpfsCid t))) .
                    (AssocMap.lookup Fixtures.catTokenIpfsCidHash)

completeAuctionDatumsCheck :: TracePredicate
completeAuctionDatumsCheck =
  dataAtAddress
    Fixtures.marketplaceAddress
    (nftNotOnAuction . Marketplace.mdSingletons)
    where
      nftNotOnAuction = maybe False (isNothing . Marketplace.nftLot) .
                        (AssocMap.lookup Fixtures.catTokenIpfsCidHash)

startAnAuctionValueCheck :: TracePredicate
startAnAuctionValueCheck =
  valueAtAddress
    (walletAddress Fixtures.userWallet)
    (isNothing . find hasNft . V.flattenValue)
    where
      hasNft v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCid

completeAnAuctionValueCheck :: TracePredicate
completeAnAuctionValueCheck =
  valueAtAddress
    (walletAddress Fixtures.userWallet)
    (Utils.one hasNft . V.flattenValue)
    where
      hasNft v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCid

buyOnAuctionValueCheck :: TracePredicate
buyOnAuctionValueCheck =
  valueAtAddress
    (walletAddress Fixtures.buyerWallet)
    (Utils.one hasNft . V.flattenValue)
    where
      hasNft v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCid

errorCheckUser :: TracePredicate
errorCheckUser = Utils.assertCrError (Marketplace.userEndpoints Fixtures.marketplace) (Trace.walletInstanceTag Fixtures.userWallet)

errorCheckBuyer :: TracePredicate
errorCheckBuyer = Utils.assertCrError (Marketplace.userEndpoints Fixtures.marketplace) (Trace.walletInstanceTag Fixtures.buyerWallet)

-- \/\/\/ "NFT bundles"
startAnAuctionParamsB ::        Marketplace.StartAnAuctionParams
startAnAuctionParamsB =  Marketplace.StartAnAuctionParams
        {
    saapItemId   = Marketplace.UserBundleId Fixtures.cids,
    saapDuration = 142
  }

closeLotParamsB ::        Marketplace.CloseLotParams
closeLotParamsB =  Marketplace.CloseLotParams {
                      clpItemId    = Marketplace.UserBundleId Fixtures.cids
                    }

bidOnAuctionParamsB :: Marketplace.BidOnAuctionParams
bidOnAuctionParamsB = Marketplace.BidOnAuctionParams {
            boapItemId = Marketplace.UserBundleId Fixtures.cids,
            boapBid    = fromInteger $ 35 * Fixtures.oneAdaInLovelace
          }

startAnAuctionTraceB :: Trace.EmulatorTrace (Trace.ContractHandle (ContractResponse Text Marketplace.UserContractState) Marketplace.MarketplaceUserSchema Void)
startAnAuctionTraceB = do
  h <- Bundles.bundleTrace

  _ <- Trace.callEndpoint @"startAnAuction" h startAnAuctionParamsB

  _ <- Trace.waitNSlots 50
  pure h

startAnAuctionTraceB' :: Trace.EmulatorTrace ()
startAnAuctionTraceB' = do
  _ <- Start.startTrace
  h <- Trace.activateContractWallet Fixtures.userWallet $ Marketplace.userEndpoints Fixtures.marketplace

  _ <- Trace.callEndpoint @"startAnAuction" h startAnAuctionParamsB

  _ <- Trace.waitNSlots 50
  pure ()

completeAnAuctionTraceB :: Trace.EmulatorTrace ()
completeAnAuctionTraceB = do
  h <- startAnAuctionTraceB

  _ <- Trace.callEndpoint @"completeAnAuction" h closeLotParamsB

  _ <- Trace.waitNSlots 250
  pure ()

bidOnAuctionTraceB :: Trace.EmulatorTrace (Trace.ContractHandle (ContractResponse Text Marketplace.UserContractState) Marketplace.MarketplaceUserSchema Void)
bidOnAuctionTraceB = do
  _ <- startAnAuctionTraceB

  h <- Trace.activateContractWallet Fixtures.buyerWallet $ Marketplace.userEndpoints Fixtures.marketplace
  _ <- Trace.callEndpoint @"bidOnAuction" h bidOnAuctionParamsB

  _ <- Trace.waitNSlots 50
  pure h

buyOnAuctionTraceB :: Trace.EmulatorTrace ()
buyOnAuctionTraceB = do
  h <- bidOnAuctionTraceB

  _ <- Trace.callEndpoint @"completeAnAuction" h closeLotParamsB

  _ <- Trace.waitNSlots 250
  pure ()

startAnAuctionDatumsCheckB :: TracePredicate
startAnAuctionDatumsCheckB =
  dataAtAddress
    Fixtures.marketplaceAddress
    (bundleIsOnAuction . Marketplace.mdBundles)
    where
      bundleIsOnAuction = maybe False (\b -> b ^. Marketplace._nbTokens ^? Marketplace._HasLot . _2 . _Right & fmap auctionValue &
                                (== Just (Marketplace.bundleValue AssocMap.empty b))) .
                          (AssocMap.lookup Fixtures.bundleId)

completeAuctionDatumsCheckB :: TracePredicate
completeAuctionDatumsCheckB =
  dataAtAddress
    Fixtures.marketplaceAddress
    (bundleNotOnAuction . Marketplace.mdBundles)
    where
      bundleNotOnAuction = maybe False (Prelude.not . Marketplace.hasLotBundle) .
                           (AssocMap.lookup Fixtures.bundleId)

startAnAuctionValueCheckB :: TracePredicate
startAnAuctionValueCheckB =
  valueAtAddress
    (walletAddress Fixtures.userWallet) $
    \v -> (isNothing . find hasCatToken . V.flattenValue $ v) && (isNothing . find hasPhotoToken . V.flattenValue $ v)
    where
      hasCatToken v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCid
      hasPhotoToken v = (v ^. _2 & V.unTokenName) == Fixtures.photoTokenIpfsCid

completeAnAuctionValueCheckB :: TracePredicate
completeAnAuctionValueCheckB =
  valueAtAddress
    (walletAddress Fixtures.userWallet) $
    \v -> (Utils.one hasCatToken . V.flattenValue $ v) && (Utils.one hasPhotoToken . V.flattenValue $ v)
    where
      hasCatToken v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCid
      hasPhotoToken v = (v ^. _2 & V.unTokenName) == Fixtures.photoTokenIpfsCid

buyOnAuctionValueCheckB :: TracePredicate
buyOnAuctionValueCheckB =
  valueAtAddress
    (walletAddress Fixtures.buyerWallet) $
    \v -> (Utils.one hasCatToken . V.flattenValue $ v) && (Utils.one hasPhotoToken . V.flattenValue $ v)
    where
      hasCatToken v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCid
      hasPhotoToken v = (v ^. _2 & V.unTokenName) == Fixtures.photoTokenIpfsCid
