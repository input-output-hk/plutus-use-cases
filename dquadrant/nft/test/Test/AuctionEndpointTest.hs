{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores#-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Test.AuctionEndpointTest
(tests)
where

import Test.TestHelper
import Ledger.Ada ( lovelaceValueOf, adaSymbol, adaToken )
import Ledger.Value as Value ( AssetClass (AssetClass), geq )
import Plutus.Contract.Test
import PlutusTx.Prelude
import Test.Tasty
import Plutus.Contract.Blockchain.MarketPlace
import Plutus.Contract.Wallet.EndpointModels
import Plutus.Trace.Emulator
import Ledger.TimeSlot (slotToPOSIXTime)
import Ledger hiding(singleton)
import qualified Control.Monad.Freer.Extras as Extras
import Prelude (show)
import Data.Functor
import qualified Data.Aeson.Types as AesonTypes
import Data.Text (Text)


tests :: TestTree
tests = testGroup "Auction"
      [ canPlaceOnAuction,
      canBidOnAuction,
      whenBid'previousBidderReceivesOldBidAmount,

      -- when winner claims
      whenWin'canClaimAuction,
      whenClaimAuctionByWinner'operatorReceivesFee,
      whenClaimAuctionByWinner'sellerReceivesPayment,

      -- when seller claims
      whenClaimAuctionBySeller'buyerReceivesAsset,
      whenClaimAuctionBySeller'sellerReceivesPayment,
      whenClaimAuctionBySeller'operatorReceivesFee ]

defaultAuctionParam :: AuctionParam
defaultAuctionParam=AuctionParam{
            apValue=toValueInfo (nft "aa") ,
            apMinBid=valueInfoLovelace 10_000_000,
            apMinIncrement=2_000_000,
            apStartTime=slotToPOSIXTime 10,
            apEndTime=slotToPOSIXTime 100
          }

canPlaceOnAuction :: TestTree
canPlaceOnAuction=
    defaultCheck
    "CanPlace OnAuction"
    (   walletFundsChange (Wallet 1) (negNft "aa")
      .&&. lockedByMarket (nft "aa")
    )$    do
      h1 <- getHandle 1
      Extras.logInfo $ "Expected Datum" ++ show expectedDatum
      Extras.logInfo $ "Actual Datum" ++ show  (aParamToAuction  ( pubKeyHash $ walletPubKey $ Wallet 1) defaultAuctionParam)
      callEndpoint @"startAuction" h1 [defaultAuctionParam]
      wait
  where

    expectedDatum=Auction {
              aOwner        = pubKeyHash (walletPubKey (Wallet 1)),
              aBidder       = pubKeyHash (walletPubKey (Wallet 1)),
              aAssetClass   = AssetClass (adaSymbol , adaToken),
              aMinBid       = 10_000_0000,
              aMinIncrement = 2_000_000,
              aDuration     =  Interval
                                  ( LowerBound  (Finite $ slotToPOSIXTime 10) True)
                                  ( UpperBound  (Finite $ slotToPOSIXTime 100) False),
              aValue        = nft "aa"
            }

canBidOnAuction :: TestTree
canBidOnAuction=defaultCheck
    "Can bid on auction"
    (         assertNoFailedTransactions
        .&&.  lockedByMarket ( nft "aa" <> lovelaceValueOf 50_000_000)
        .&&.  walletFundsChange (Wallet 2) (lovelaceValueOf (-50_000_000))
    )$    do
      h1 <-getHandle 1
      h2 <-getHandle 2
      h3 <- getHandle 3
      defaultAuctionUtxo h1
        >>= doBid h2 15_000_000
        >>= doBid h3 30_000_000
        >>= doBid h2 50_000_000
        >> wait


whenBid'previousBidderReceivesOldBidAmount :: TestTree
whenBid'previousBidderReceivesOldBidAmount=
  defaultCheck
    "Last Bidder Receives its bid on new bid"
    (         assertNoFailedTransactions
        .&&.  walletFundsChange (Wallet 2) (lovelaceValueOf 0)
        .&&. walletFundsChange (Wallet 3) (lovelaceValueOf 0)
        .&&. walletFundsChange (Wallet 4) (lovelaceValueOf (-500_000_000))
    )$    do
      h1<- getHandle 1
      h2 <-getHandle 2
      h3 <- getHandle 3
      h4 <-getHandle 4

      defaultAuctionUtxo h1
        >>= doBid h4 10_000_000
        >>= doBid h3 20_000_000
        >>= doBid h2 30_000_000
        >>= doBid h4 50_000_000
        >>= doBid h3 100_000_000
        >>= doBid h4 500_000_000
        >> wait

  where

whenWin'canClaimAuction :: TestTree
whenWin'canClaimAuction=
  defaultCheck
    "Can claim won auction"
    (         assertNoFailedTransactions
        .&&.  valueAtAddress (pubKeyAddress $ walletPubKey $ Wallet 3) (\v-> v `geq` nft "aa")
        .&&. lockedByMarket (lovelaceValueOf 0)
    )$    do
      h1 <- getHandle 1
      h2 <-getHandle 2
      h3 <- getHandle 3
      defaultAuctionUtxo h1
        >>=doBid h2 10_000_000
        >>=doBid h3 30_000_000
        >>=doClaim h3
        >> wait

whenClaimAuctionBySeller'buyerReceivesAsset :: TestTree
whenClaimAuctionBySeller'buyerReceivesAsset=
  defaultCheck
    "On Claiming Auction By Seller, buyer receives Asset"
    (   assertNoFailedTransactions
    .&&. walletHasAtLeast  (nft "aa") 2
    .&&. lockedByMarket (lovelaceValueOf 0)
    )$    do
      h1 <- getHandle 1
      h2<-getHandle 2
      h3 <-getHandle 3
      defaultAuctionUtxo h1
        >>=doBid h2 100_000_000
        >>=doBid h3 200_000_000
        >>=doBid h2 300_000_000
        >>= doClaim h1

whenClaimAuctionByWinner'operatorReceivesFee :: TestTree
whenClaimAuctionByWinner'operatorReceivesFee=
  defaultCheck
    "On Claiming Auction, Operator Receives Fee"
    (         assertNoFailedTransactions
        .&&. walletFundsChange operator  (lovelaceValueOf 6_000_000)
    )$    do
      h1<- getHandle 1
      h2 <-getHandle 2
      h3<-getHandle 3
      defaultAuctionUtxo h1
        >>=doBid h2 100_000_000
        >>=doBid h3 200_000_000
        >>=doBid h2 600_000_000
        >>= doClaim h2
        >> wait

whenClaimAuctionBySeller'operatorReceivesFee :: TestTree
whenClaimAuctionBySeller'operatorReceivesFee=
  defaultCheck
    "When Seller claims auction, Operator Receives Fee"
    (         assertNoFailedTransactions
        .&&. walletFundsChange operator  (lovelaceValueOf 6_000_000)
    )$    do
      h1<- getHandle 1
      h2 <-getHandle 2
      h3<-getHandle 3
      defaultAuctionUtxo h1
        >>=doBid h2 100_000_000
        >>=doBid h3 200_000_000
        >>=doBid h2 600_000_000
        >>= doClaim h1
        >> wait

whenClaimAuctionByWinner'sellerReceivesPayment :: TestTree
whenClaimAuctionByWinner'sellerReceivesPayment=
  defaultCheck
    "When winner claims auction, Seller Receives Payment"
    (   assertNoFailedTransactions
    .&&.walletFundsChange (Wallet 1) (lovelaceValueOf 495_000_000 <> negNft "aa")
    )$    do
      h1 <- getHandle 1
      h2<-getHandle 2
      h3 <-getHandle 3
      defaultAuctionUtxo h1
        >>=doBid h2 100_000_000
        >>=doBid h3 300_000_000
        >>=doBid h2 500_000_000
        >>= doClaim h2

whenClaimAuctionBySeller'sellerReceivesPayment :: TestTree
whenClaimAuctionBySeller'sellerReceivesPayment=
  defaultCheck
    "When Seller Claims auction, Seller Receives Payment"
    (   assertNoFailedTransactions
    .&&.walletFundsChange (Wallet 1) (lovelaceValueOf 495_000_000 <> negNft "aa")
    )$    do
      h1 <- getHandle 1
      h2<-getHandle 2
      h3 <-getHandle 3
      defaultAuctionUtxo h1
        >>=doBid h2 100_000_000
        >>=doBid h3 300_000_000
        >>=doBid h2 500_000_000
        >>= doClaim h1


defaultAuctionUtxo :: ContractHandle [AesonTypes.Value] TestSchema Text -> EmulatorTrace [TxOutRef]
defaultAuctionUtxo h =do
      callEndpoint @"startAuction" h [defaultAuctionParam]
      u1<-waitForLastUtxos h
      void $ waitUntilSlot 10
      return  u1


doBid:: ContractHandle [AesonTypes.Value] TestSchema Text -> Integer -> [TxOutRef]->EmulatorTrace [TxOutRef]
doBid h v u= do
  callEndpoint @"bid" h $ BidParam (head u) [valueInfoLovelace v]
  waitForLastUtxos h

doClaim:: ContractHandle [AesonTypes.Value] TestSchema Text -> [TxOutRef]->EmulatorTrace ()
doClaim h u = do
  void $ waitUntilSlot 100
  callEndpoint @"claim" h $ ClaimParam u True