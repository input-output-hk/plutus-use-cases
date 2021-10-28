{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Marketplace.Spec.Auction
  ( tests
  ) where

import           Control.Lens                                 (_2, _Right, (&),
                                                               (^.), (^?))
import           Control.Monad                                (void)
import           Data.Foldable                                (find)
import           Data.Maybe                                   (isNothing)
import           Data.Proxy
import           Data.Text                                    (Text)
import           Data.Void                                    (Void)
import           Ledger                                       (Value)
import           Ledger.Ada                                   (lovelaceValueOf)
import qualified Ledger.Value                                 as V
import qualified Marketplace.Fixtures                         as Fixtures
import qualified Marketplace.Spec.Bundles                     as Bundles
import qualified Marketplace.Spec.CreateNft                   as CreateNft
import qualified Marketplace.Spec.Start                       as Start
import           Plutus.Abstract.ContractResponse             (ContractResponse)
import           Plutus.Contract.Test
import qualified Plutus.Contracts.NftMarketplace.Endpoints    as Marketplace
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Marketplace
import qualified Plutus.Contracts.Services.Auction.Core       as Auction
import qualified Plutus.Trace                                 as Trace
import           Plutus.V1.Ledger.Time                        (DiffMilliSeconds (..),
                                                               POSIXTime (..),
                                                               fromMilliSeconds)
import qualified PlutusTx.AssocMap                            as AssocMap
import           Test.Tasty
import qualified Utils
import           Wallet.Emulator.Wallet                       (walletAddress)

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
        errorCheckStart
        startAnAuctionTrace',
      checkPredicateOptions
        Fixtures.options
        "Should close auction and pay locked NFT back if there were no bids"
        (completeAnAuctionValueCheck .&&. completeAuctionDatumsCheck)
        completeAnAuctionTrace,
      checkPredicateOptions
        Fixtures.options
        "Should not close auction if it was not started"
        errorCheckComplete
        completeAnAuctionTrace',
      checkPredicateOptions
        Fixtures.options
        "Should bid on NFT"
        startAnAuctionDatumsCheck
        (void bidOnAuctionTrace),
      checkPredicateOptions
        Fixtures.options
        "Should not bid if NFT is not on auction"
        errorCheckBid
        bidOnAuctionTrace',
      checkPredicateOptions
        Fixtures.options
        "Should close auction and pay locked NFT to the highest bidder"
        (buyOnAuctionValueCheck .&&. completeAuctionDatumsCheck)
        buyOnAuctionTrace,
      checkPredicateOptions
        Fixtures.options
        "Should close auction and pay marketplace operator a saleFee"
        (marketplaceOperatorFeeCheck .&&. sellerProfitWithFeeCheck)
        buyOnAuctionTrace,
      checkPredicateOptions
        Fixtures.options
        "Should cancel auction and get NFT back to owner when no bids"
        (cancelAuctionDatumsCheck .&&. cancelAnAuctionValueCheck)
        cancelAuctionWithoutBidsTrace,
      checkPredicateOptions
        Fixtures.options
        "Should cancel auction and get NFT and bid back to owner and bidder"
        (cancelAuctionDatumsCheck .&&. cancelAnAuctionValueCheck .&&. cancelAnAuctionBidReturningCheck)
        cancelAuctionWithBidsTrace,
      checkPredicateOptions
        Fixtures.options
        "Should not cancel auction if time is over"
        errorCheckCancel
        cancelAuctionWhenTimeIsOverTrace
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
        errorCheckStart
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
        buyOnAuctionTraceB,
      checkPredicateOptions
        Fixtures.options
        "Should close bundle auction and pay marketplace operator a saleFee"
        (marketplaceOperatorFeeCheckB .&&. sellerProfitWithFeeCheckB)
        buyOnAuctionTraceB,
      checkPredicateOptions
        Fixtures.options
        "Should cancel auction and get bundle back to owner when no bids"
        (cancelAuctionDatumsCheckB .&&. cancelAnAuctionValueCheckB)
        cancelAuctionWithoutBidsTraceB,
      checkPredicateOptions
        Fixtures.options
        "Should cancel auction and get bundle and bid back to owner and bidder"
        (cancelAuctionDatumsCheckB .&&. cancelAnAuctionValueCheckB .&&. cancelAnAuctionBidReturningCheckB)
        cancelAuctionWithBidsTraceB
    ]]

-- Setted up for Simulation and Emulators in plutus Ledger.TimeSlot module
beginningOfTime :: Integer
beginningOfTime = 1596059091000

-- \/\/\/ "NFT singletons"
startAnAuctionParams ::        Marketplace.StartAnAuctionParams
startAnAuctionParams =  Marketplace.StartAnAuctionParams
        {
    Marketplace.saapItemId   = Marketplace.UserNftId Fixtures.catTokenIpfsCid,
    Marketplace.saapEndTime = (POSIXTime beginningOfTime) + fromMilliSeconds (DiffMilliSeconds (155 * 1000)),
    Marketplace.saapInitialPrice = fromInteger $ 5 * Fixtures.oneAdaInLovelace
  }

closeLotParams ::        Marketplace.CloseLotParams
closeLotParams =  Marketplace.CloseLotParams {
                      Marketplace.clpItemId    = Marketplace.UserNftId Fixtures.catTokenIpfsCid
                    }
bidOnAuctionParams :: Marketplace.BidOnAuctionParams
bidOnAuctionParams = Marketplace.BidOnAuctionParams {
            Marketplace.boapItemId = Marketplace.UserNftId Fixtures.catTokenIpfsCid,
            Marketplace.boapBid    = fromInteger $ 25 * Fixtures.oneAdaInLovelace
          }

startAnAuctionTrace :: Trace.EmulatorTrace (Trace.ContractHandle (ContractResponse String Text Marketplace.UserContractState) Marketplace.MarketplaceUserSchema Void)
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

bidOnAuctionTrace :: Trace.EmulatorTrace (Trace.ContractHandle (ContractResponse String Text Marketplace.UserContractState) Marketplace.MarketplaceUserSchema Void)
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

cancelAuctionWithBidsTrace :: Trace.EmulatorTrace ()
cancelAuctionWithBidsTrace = do
  h <- startAnAuctionTrace

  h1 <- Trace.activateContractWallet Fixtures.buyerWallet $ Marketplace.userEndpoints Fixtures.marketplace
  _ <- Trace.callEndpoint @"bidOnAuction" h1 bidOnAuctionParams
  _ <- Trace.callEndpoint @"cancelAnAuction" h closeLotParams

  _ <- Trace.waitNSlots 50
  pure ()

cancelAuctionWithoutBidsTrace :: Trace.EmulatorTrace ()
cancelAuctionWithoutBidsTrace = do
  h <- startAnAuctionTrace
  _ <- Trace.callEndpoint @"cancelAnAuction" h closeLotParams

  _ <- Trace.waitNSlots 50
  pure ()

cancelAuctionWhenTimeIsOverTrace :: Trace.EmulatorTrace ()
cancelAuctionWhenTimeIsOverTrace = do
  h <- CreateNft.createNftTrace
  let startAuctionParamsWithLessTime = startAnAuctionParams {Marketplace.saapEndTime = (POSIXTime beginningOfTime) + fromMilliSeconds (DiffMilliSeconds (5 * 1000))}
  
  _ <- Trace.callEndpoint @"startAnAuction" h startAuctionParamsWithLessTime 

  _ <- Trace.waitNSlots 50

  _ <- Trace.callEndpoint @"cancelAnAuction" h closeLotParams

  _ <- Trace.waitNSlots 50
  pure ()

startAnAuctionDatumsCheck :: TracePredicate
startAnAuctionDatumsCheck =
  dataAtAddress
    Fixtures.marketplaceAddress
    (Utils.checkOneDatum (nftIsOnAuction . Marketplace.mdSingletons))
    where
      nftIsOnAuction = maybe False (\t -> Marketplace.getAuctionFromNFT t & fmap Auction.aAsset &
                                (== Just (Marketplace.nftValue Fixtures.catTokenIpfsCidBs t))) .
                    AssocMap.lookup Fixtures.catTokenIpfsCidHash

completeAuctionDatumsCheck :: TracePredicate
completeAuctionDatumsCheck =
  dataAtAddress
    Fixtures.marketplaceAddress
    (Utils.checkOneDatum (nftNotOnAuction . Marketplace.mdSingletons))
    where
      nftNotOnAuction = maybe False (isNothing . Marketplace.nftLot) .
                        AssocMap.lookup Fixtures.catTokenIpfsCidHash

startAnAuctionValueCheck :: TracePredicate
startAnAuctionValueCheck =
  valueAtAddress
    (walletAddress Fixtures.userWallet)
    (isNothing . find hasNft . V.flattenValue)
    where
      hasNft v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCidBs

completeAnAuctionValueCheck :: TracePredicate
completeAnAuctionValueCheck =
  valueAtAddress
    (walletAddress Fixtures.userWallet)
    (Utils.one hasNft . V.flattenValue)
    where
      hasNft v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCidBs

cancelAuctionDatumsCheck :: TracePredicate
cancelAuctionDatumsCheck =
  dataAtAddress
    Fixtures.marketplaceAddress
    (Utils.checkOneDatum (nftNotOnAuction . Marketplace.mdSingletons))
    where
      nftNotOnAuction = maybe False (isNothing . Marketplace.nftLot) .
                        AssocMap.lookup Fixtures.catTokenIpfsCidHash

cancelAnAuctionValueCheck :: TracePredicate
cancelAnAuctionValueCheck =
  valueAtAddress
    (walletAddress Fixtures.userWallet)
    (Utils.one hasNft . V.flattenValue)
    where
      hasNft v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCidBs

cancelAnAuctionBidReturningCheck :: TracePredicate
cancelAnAuctionBidReturningCheck =
  walletFundsChange Fixtures.buyerWallet $ lovelaceValueOf 0

buyOnAuctionValueCheck :: TracePredicate
buyOnAuctionValueCheck =
  valueAtAddress
    (walletAddress Fixtures.buyerWallet)
    (Utils.one hasNft . V.flattenValue)
    where
      hasNft v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCidBs

errorCheckStart :: TracePredicate
errorCheckStart = Utils.assertCrError (Proxy @"startAnAuction") (Marketplace.userEndpoints Fixtures.marketplace) (Trace.walletInstanceTag Fixtures.userWallet)

errorCheckComplete :: TracePredicate
errorCheckComplete = Utils.assertCrError (Proxy @"completeAnAuction") (Marketplace.userEndpoints Fixtures.marketplace) (Trace.walletInstanceTag Fixtures.userWallet)

errorCheckBid :: TracePredicate
errorCheckBid = Utils.assertCrError (Proxy @"bidOnAuction") (Marketplace.userEndpoints Fixtures.marketplace) (Trace.walletInstanceTag Fixtures.buyerWallet)

errorCheckCancel :: TracePredicate
errorCheckCancel = Utils.assertCrError (Proxy @"cancelAnAuction") (Marketplace.userEndpoints Fixtures.marketplace) (Trace.walletInstanceTag Fixtures.userWallet)

-- \/\/\/ "NFT bundles"
startAnAuctionParamsB ::        Marketplace.StartAnAuctionParams
startAnAuctionParamsB =  Marketplace.StartAnAuctionParams
        {
    Marketplace.saapItemId   = Marketplace.UserBundleId Fixtures.cids,
    Marketplace.saapEndTime = (POSIXTime beginningOfTime) + fromMilliSeconds (DiffMilliSeconds (300 * 1000)),
    Marketplace.saapInitialPrice = fromInteger $ 15 * Fixtures.oneAdaInLovelace
  }

closeLotParamsB ::        Marketplace.CloseLotParams
closeLotParamsB =  Marketplace.CloseLotParams {
                      Marketplace.clpItemId    = Marketplace.UserBundleId Fixtures.cids
                    }

bidOnAuctionParamsB :: Marketplace.BidOnAuctionParams
bidOnAuctionParamsB = Marketplace.BidOnAuctionParams {
            Marketplace.boapItemId = Marketplace.UserBundleId Fixtures.cids,
            Marketplace.boapBid    = fromInteger $ 35 * Fixtures.oneAdaInLovelace
          }

startAnAuctionTraceB :: Trace.EmulatorTrace (Trace.ContractHandle (ContractResponse String Text Marketplace.UserContractState) Marketplace.MarketplaceUserSchema Void)
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

bidOnAuctionTraceB :: Trace.EmulatorTrace (Trace.ContractHandle (ContractResponse String Text Marketplace.UserContractState) Marketplace.MarketplaceUserSchema Void)
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

cancelAuctionWithBidsTraceB :: Trace.EmulatorTrace ()
cancelAuctionWithBidsTraceB = do
  h <- startAnAuctionTraceB

  h1 <- Trace.activateContractWallet Fixtures.buyerWallet $ Marketplace.userEndpoints Fixtures.marketplace
  _ <- Trace.callEndpoint @"bidOnAuction" h1 bidOnAuctionParamsB
  _ <- Trace.callEndpoint @"cancelAnAuction" h closeLotParamsB

  _ <- Trace.waitNSlots 50
  pure ()

cancelAuctionWithoutBidsTraceB :: Trace.EmulatorTrace ()
cancelAuctionWithoutBidsTraceB = do
  h <- startAnAuctionTraceB
  _ <- Trace.callEndpoint @"cancelAnAuction" h closeLotParamsB

  _ <- Trace.waitNSlots 50
  pure ()

startAnAuctionDatumsCheckB :: TracePredicate
startAnAuctionDatumsCheckB =
  dataAtAddress
    Fixtures.marketplaceAddress
    (Utils.checkOneDatum (bundleIsOnAuction . Marketplace.mdBundles))
    where
      bundleIsOnAuction = maybe False (\b -> Marketplace.getAuctionFromBundle b & fmap Auction.aAsset &
                                (== Just (Marketplace.bundleValue AssocMap.empty b))) .
                          AssocMap.lookup Fixtures.bundleId

completeAuctionDatumsCheckB :: TracePredicate
completeAuctionDatumsCheckB =
  dataAtAddress
    Fixtures.marketplaceAddress
    (Utils.checkOneDatum (bundleNotOnAuction . Marketplace.mdBundles))
    where
      bundleNotOnAuction = maybe False (Prelude.not . Marketplace.hasLotBundle) .
                           AssocMap.lookup Fixtures.bundleId

startAnAuctionValueCheckB :: TracePredicate
startAnAuctionValueCheckB =
  valueAtAddress
    (walletAddress Fixtures.userWallet) $
    \v -> (isNothing . find hasCatToken . V.flattenValue $ v) && (isNothing . find hasPhotoToken . V.flattenValue $ v)
    where
      hasCatToken v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCidBs
      hasPhotoToken v = (v ^. _2 & V.unTokenName) == Fixtures.photoTokenIpfsCidBs

completeAnAuctionValueCheckB :: TracePredicate
completeAnAuctionValueCheckB =
  valueAtAddress
    (walletAddress Fixtures.userWallet) $
    \v -> (Utils.one hasCatToken . V.flattenValue $ v) && (Utils.one hasPhotoToken . V.flattenValue $ v)
    where
      hasCatToken v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCidBs
      hasPhotoToken v = (v ^. _2 & V.unTokenName) == Fixtures.photoTokenIpfsCidBs

cancelAuctionDatumsCheckB :: TracePredicate
cancelAuctionDatumsCheckB =
  dataAtAddress
    Fixtures.marketplaceAddress
    (Utils.checkOneDatum (bundleNotOnAuction . Marketplace.mdBundles))
    where
      bundleNotOnAuction = maybe False (Prelude.not . Marketplace.hasLotBundle) .
                           AssocMap.lookup Fixtures.bundleId

cancelAnAuctionValueCheckB :: TracePredicate
cancelAnAuctionValueCheckB =
  valueAtAddress
    (walletAddress Fixtures.userWallet) $
    \v -> (Utils.one hasCatToken . V.flattenValue $ v) && (Utils.one hasPhotoToken . V.flattenValue $ v)
    where
      hasCatToken v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCidBs
      hasPhotoToken v = (v ^. _2 & V.unTokenName) == Fixtures.photoTokenIpfsCidBs

cancelAnAuctionBidReturningCheckB :: TracePredicate
cancelAnAuctionBidReturningCheckB =
  walletFundsChange Fixtures.buyerWallet $ lovelaceValueOf 0

buyOnAuctionValueCheckB :: TracePredicate
buyOnAuctionValueCheckB =
  valueAtAddress
    (walletAddress Fixtures.buyerWallet) $
    \v -> (Utils.one hasCatToken . V.flattenValue $ v) && (Utils.one hasPhotoToken . V.flattenValue $ v)
    where
      hasCatToken v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCidBs
      hasPhotoToken v = (v ^. _2 & V.unTokenName) == Fixtures.photoTokenIpfsCidBs

marketplaceOperatorFeeCheck :: TracePredicate
marketplaceOperatorFeeCheck =
  walletFundsChange Fixtures.ownerWallet $ lovelaceValueOf 725000
  -- 25000000 * 2.5 /100 = 625000 - fee by complete auction
  -- 100000 - fee by minting token

sellerProfitWithFeeCheck :: TracePredicate
sellerProfitWithFeeCheck =
  walletFundsChange Fixtures.userWallet $ lovelaceValueOf 24275000
  -- 25000000 - 725000 = 24275000

marketplaceOperatorFeeCheckB :: TracePredicate
marketplaceOperatorFeeCheckB =
  walletFundsChange Fixtures.ownerWallet $ lovelaceValueOf 1175000
  -- 35000000 * 2.5 /100 = 875000 - fee by complete auction
  -- 100000 * 2 = 200000 - fee by minting 2 tokens
  -- 100000 - fee by bundling

sellerProfitWithFeeCheckB :: TracePredicate
sellerProfitWithFeeCheckB =
  walletFundsChange Fixtures.userWallet $ lovelaceValueOf 33825000
  -- 35000000 - 1175000 = 33825000
