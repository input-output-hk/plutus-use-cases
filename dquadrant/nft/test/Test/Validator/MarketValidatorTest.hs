{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.Validator.MarketValidatorTest
-- (tests)
where

import Test.Tasty ( TestTree, defaultMain, testGroup, TestName )
import Test.Tasty.HUnit
import Test.ValidatorTestFramework
import Plutus.Contract.Blockchain.MarketPlace
import Test.TestHelper
import Ledger.Value

import PlutusTx (fromData)
import Ledger
import PlutusTx.Prelude
import Plutus.Contract.Test
import Ledger.Ada

main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Market Validator Unit Test" [
    testGroup "Buy Redeemer" [
      execMarket "Can buy single item" (
            builderRedeem Buy (nft "a") defaultSale
        <>  builderSpend (Wallet 2) (lovelaceValueOf 100_000_000)
        <>  builderPayTo (Wallet 1) (lovelaceValueOf 98_000_000)
        <>  builderPayTo (Wallet 2) (nft "a")
        <>  builderPayTo operator (lovelaceValueOf 2_000_000)
      ),
      execMarket "Can buy and gift to another wallet" (
            builderRedeem Buy (nft "a") defaultSale
        <>  builderSpend (Wallet 2) (lovelaceValueOf 100_000_000)
        <>  builderPayTo (Wallet 1) (lovelaceValueOf 98_000_000)
        <>  builderPayTo (Wallet 3) (nft "a")
        <>  builderPayTo operator (lovelaceValueOf 2_000_000)
      ),
      execMarket "Can buy multiple items " (
            builderRedeem Buy (nft "a") (primarySale (Wallet 1) (priveInLovelace 100_000_000))
        <>  builderRedeem Buy (nft "b") (primarySale (Wallet 2) (priveInLovelace 200_000_000))
        <>  builderRedeem Buy (nft "c") (primarySale (Wallet 3) (priveInLovelace 300_000_000))
        <>  builderSpend (Wallet 4) (lovelaceValueOf 600_000_000)
        <>  builderPayTo (Wallet 1) (lovelaceValueOf 98_000_000)
        <>  builderPayTo (Wallet 2) (lovelaceValueOf 196_000_000)
        <>  builderPayTo (Wallet 3) (lovelaceValueOf 294_000_000)
        <>  builderPayTo (Wallet 2) (nft "a"<>nft"b"<>nft"c")
        <>  builderPayTo operator (lovelaceValueOf 12_000_000)
      ),

      execFailMarket "Cannot Pay less to seller" (
            builderRedeem Buy (nft "a") defaultSale
        <>  builderSpend (Wallet 2) (lovelaceValueOf 99_000_000)
        <>  builderPayTo (Wallet 1) (lovelaceValueOf 97_000_000)
        <>  builderPayTo (Wallet 2) (nft "a")
        <>  builderPayTo operator (lovelaceValueOf 2_000_000)
      ),
      execFailMarket "Cannot Pay less fees" (
            builderRedeem Buy (nft "a") defaultSale
        <>  builderSpend (Wallet 2) (lovelaceValueOf 99_000_000)
        <>  builderPayTo (Wallet 1) (lovelaceValueOf 98_000_000)
        <>  builderPayTo (Wallet 2) (nft "a")
        <>  builderPayTo operator (lovelaceValueOf 1_000_000)
      ),
      execMarket "Can give tips to the seller" (
            builderRedeem Buy (nft "a") defaultSale
        <>  builderSpend (Wallet 2) (lovelaceValueOf 110_000_000)
        <>  builderPayTo (Wallet 1) (lovelaceValueOf 108_000_000)
        <>  builderPayTo (Wallet 2) (nft "a")
        <>  builderPayTo operator (lovelaceValueOf 2_000_000)
      ),
       execMarket "Can give tips to the market" (
            builderRedeem Buy (nft "a") defaultSale
        <>  builderSpend (Wallet 2) (lovelaceValueOf 110_000_000)
        <>  builderPayTo (Wallet 1) (lovelaceValueOf 108_000_000)
        <>  builderPayTo (Wallet 2) (nft "a")
        <>  builderPayTo operator (lovelaceValueOf 12_000_000)
      ),
      execFailMarket "Cannot double satisfy on identical sale" (
            builderRedeem Buy (nft "a") (primarySale (Wallet 1) (priveInLovelace 100_000_000))
        <>  builderRedeem Buy (nft "a") (primarySale (Wallet 1) (priveInLovelace 100_000_000))
        <>  builderSpend (Wallet 2) (lovelaceValueOf 100_000_000)
        <>  builderPayTo (Wallet 1) (lovelaceValueOf 98_000_000)
        <>  builderPayTo (Wallet 2)  (cardanoToken "a" 2)
        <>  builderPayTo operator (lovelaceValueOf 2_000_000)
      ),
      execFailMarket "Cannot double satisfy payment to seller" (
            builderRedeem Buy (nft "a") (primarySale (Wallet 1) (priveInLovelace 100_000_000))
        <>  builderRedeem Buy (nft "b") (primarySale (Wallet 1) (priveInLovelace 100_000_000))
        <>  builderSpend (Wallet 2) (lovelaceValueOf 102_000_000)
        <>  builderPayTo (Wallet 1) (lovelaceValueOf 98_000_000)
        <>  builderPayTo (Wallet 2)  (nft "a"<>nft "b")
        <>  builderPayTo operator (lovelaceValueOf 4_000_000)
      ),
      execFailMarket "Cannot double satisfy marketFee" (
            builderRedeem Buy (nft "a") (primarySale (Wallet 1) (priveInLovelace 100_000_000))
        <>  builderRedeem Buy (nft "b") (primarySale (Wallet 3) (priveInLovelace 100_000_000))
        <>  builderSpend (Wallet 2) (lovelaceValueOf 198_000_000)
        <>  builderPayTo (Wallet 1) (lovelaceValueOf  98_000_000)
        <>  builderPayTo (Wallet 3) (lovelaceValueOf  98_000_000)
        <>  builderPayTo (Wallet 2)  (nft "a"<>nft "b")
        <>  builderPayTo operator (lovelaceValueOf 2_000_000)
      )
    ],
    testGroup "Withdraw Redeemer" [
      execMarket "Can Withdraw single sale" (
            builderRedeem Withdraw (nft "a") defaultSale
        <>  builderPayTo (Wallet 1) (nft "a")
        <>  builderSign (Wallet 1)
      ),
      execMarket "Can Withdraw multiple sales"(
        builderRedeem Withdraw (nft "a") (primarySale (Wallet 2) (priveInLovelace 100_000_000))
        <>  builderRedeem Withdraw (nft "b") (primarySale (Wallet 2) (priveInLovelace 200_000_000))
        <>  builderPayTo (Wallet 2) (nft "a" <> nft "b")
        <> builderSign (Wallet 2)
      ),
      execFailMarket "Cannot withdraw without seller signature" (
        builderRedeem Withdraw (nft "a") (primarySale (Wallet 1) (priveInLovelace 100_000_000))
        <>  builderPayTo (Wallet 1) (nft "a")
        <> builderSign (Wallet 2)
      ),
      execFailMarket "Cannot withdraw my mixing with own item" (
            builderRedeem Withdraw (nft  "a") (primarySale (Wallet 2) (priveInLovelace 100_000_000))
        <>  builderRedeem Withdraw (nft "a") (primarySale (Wallet 1) (priveInLovelace 100_000_000))
        <>  builderPayTo (Wallet 2) (cardanoToken "a" 2)
        <>  builderSign (Wallet 2)
      ),
      execMarketTimed "Can Withdraw before Auction starts" (
            builderRedeem Withdraw (nft "a") defaultW1sAuction
        <>  builderPayTo (Wallet 1) (nft "a")
        <>  builderSign (Wallet 1)
      ) (startInclusiveEndExclusiveRange 500 900),
      execFailMarket "Cannot withdraw auction without auctioner's signature"(
            builderRedeem Withdraw (nft "a") defaultW1sAuction
        <>  builderPayTo (Wallet 1) (nft "a")
        <>  builderSign (Wallet 2)
      ),
      execMarketTimed "Can Withdraw after Auction ends" (
            builderRedeem Withdraw (nft "a") defaultW1sAuction
        <>  builderPayTo (Wallet 1) (nft "a")
        <>  builderSign (Wallet 1)
      )(startInclusiveEndExclusiveRange 2000 3000),
      execMarketTimed "Can withdraw during exact Auction interval if no bid" (
            builderRedeem Withdraw (nft "a") defaultW1sAuction
        <>  builderPayTo (Wallet 1) (nft "a")
        <>  builderSign (Wallet 1)
      ) (startInclusiveEndExclusiveRange 1000 2000),
      execMarketTimed "Can withdraw during Auctions' subset internal interval if no bid" (
            builderRedeem Withdraw (nft "a") defaultW1sAuction
        <>  builderPayTo (Wallet 1) (nft "a")
        <>  builderSign (Wallet 1)
      ) (startInclusiveEndExclusiveRange 1400 1600),
      execFailMarketTimed "Cannot withdraw during Auctions with bidder's signature" (
            builderRedeem Withdraw (nft "a") (defaultBid $ Wallet 2)
        <>  builderPayTo (Wallet 2) (nft "a")
        <>  builderSign (Wallet 2)
      ) (startInclusiveEndExclusiveRange 1400 1600),
      execFailMarketTimed "Cannot withdraw during exact Auction interval if there's bid" (
            builderRedeem Withdraw (nft "a") defaultW1sAuction
        <>  builderPayTo (Wallet 1) (nft "a")
        <>  builderSign (Wallet 1)
      ) (startInclusiveEndExclusiveRange 1000 2000),
      execFailMarketTimed "Cannot withdraw during Auction subset interval if there's bid" (
            builderRedeem Withdraw (nft "a") defaultW1sAuction
        <>  builderPayTo (Wallet 1) (nft "a")
        <>  builderSign (Wallet 1)
      ) (startInclusiveEndExclusiveRange 1300 1900),
      execMarket "Can withdraw auction and directsale in same transaction" (
            builderRedeem Withdraw (nft "a") defaultW1sAuction
        <>  builderRedeem Withdraw (nft "b") defaultSale
        <>  builderPayTo (Wallet 1) (nft "a"<>nft "b")
        <>  builderSign (Wallet 1)
      )
    ],
    testGroup "Bid Redeemer" [
      execMarketTimed "Can make first bid during auction"(
            builderRedeem Bid (nft "a") defaultW1sAuction
        <>  builderSpend (Wallet 2) (lovelaceValueOf 100_000_000)
        <>  builderLockInThisScript (lovelaceValueOf 100_000_000<> nft "a") (defaultBid $ Wallet 2)
      ) (startInclusiveEndExclusiveRange 1500 1600),
      execMarketTimed "Can make second bid during auction"(
            builderRedeem Bid (nft "a"<>lovelaceValueOf 200_000_000) (defaultBid $ Wallet 2)
        <>  builderSpend (Wallet 3) (lovelaceValueOf 210_000_000)
        <>  builderLockInThisScript (lovelaceValueOf 210_000_000<> nft "a") (defaultBid $ Wallet 3)
        <> builderPayLovelaceTo (Wallet 2) 200_000_000
      ) (startInclusiveEndExclusiveRange 1500 1600),
      execFailMarketTimed "Cannot Bid less than minimun bid" (
            builderRedeem Bid (nft "a") defaultW1sAuction
        <>  builderSpend (Wallet 2) (lovelaceValueOf 98_000_000)
        <>  builderLockInThisScript (lovelaceValueOf 98_000_000<> nft "a") (defaultBid $ Wallet 2)
      ) (startInclusiveEndExclusiveRange 1000 2000),
      execFailMarketTimed "Cannot Bid less than minimun increment" (
            builderRedeem Bid (nft "a"<>lovelaceValueOf 200_000_000) (defaultBid $ Wallet 2)
        <>  builderSpend (Wallet 3) (lovelaceValueOf 209_000_000)
        <>  builderLockInThisScript (lovelaceValueOf 209_000_000<> nft "a") (defaultBid $ Wallet 3)
        <>  builderPayTo (Wallet 2) (lovelaceValueOf 200_000_000)
      ) (startInclusiveEndExclusiveRange 1000 2000),
      execFailMarketTimed "Cannot pay less to previous bidder " (
            builderRedeem Bid (nft "a"<>lovelaceValueOf 200_000_000) (defaultBid $ Wallet 2)
        <>  builderSpend (Wallet 3) (lovelaceValueOf 10_000_000)
        <>  builderLockInThisScript (lovelaceValueOf 210_000_000<> nft "a") (defaultBid $ Wallet 3)
      ) (startInclusiveEndExclusiveRange 1000 2000),
      execMarketTimed "Can  Bid more than miminum bid"(
            builderRedeem Bid (nft "a") defaultW1sAuction
        <>  builderSpend (Wallet 2) (lovelaceValueOf 1_000_000_000)
        <>  builderLockInThisScript (lovelaceValueOf 1_000_000_000 <> nft "a") (defaultBid $ Wallet 2)
      ) (startInclusiveEndExclusiveRange 1000 2000),
      execFailMarketTimed "Cannot bid before auction"(
            builderRedeem Bid (nft "a") defaultW1sAuction
        <>  builderSpend (Wallet 2) (lovelaceValueOf 1_000_000_000)
        <>  builderLockInThisScript (lovelaceValueOf 1_000_000_000 <> nft "a") (defaultBid $ Wallet 2)
      ) (startInclusiveEndExclusiveRange 500 1000),
      execFailMarketTimed "Cannot bid after auction"(
            builderRedeem Bid (nft "a") defaultW1sAuction
        <>  builderSpend (Wallet 2) (lovelaceValueOf 1_000_000_000)
        <>  builderLockInThisScript (lovelaceValueOf 1_000_000_000 <> nft "a") (defaultBid $ Wallet 2)
      ) (startInclusiveEndExclusiveRange 2000 4000),
      execFailMarketTimed "Cann't bid if you are owner"(
            builderRedeem Bid (nft "a") defaultW1sAuction
        <>  builderSpend (Wallet 1) (lovelaceValueOf 100_000_000)
        <>  builderLockInThisScript (lovelaceValueOf 100_000_000 <> nft "a") defaultW1sAuction
      ) (startInclusiveEndExclusiveRange 1000 2000)

    ],
    testGroup "ClaimAuction Redeemer" [
      execMarketTimed "Can Claim after auction"(
           builderRedeem ClaimBid  (nft "a"<>lovelaceValueOf 100_000_000) (defaultBid $ Wallet 2)
        <> builderPayTo ( Wallet 2) (nft "a")
        <> builderPayTo (Wallet 1) (lovelaceValueOf 99_000_000)
        <> builderPayTo operator (lovelaceValueOf  1_000_000)
        <> builderSign (Wallet 2)
      ) (startInclusiveEndExclusiveRange 2000 25000),
      execFailMarketTimed "Cannot Claim during auction"(
           builderRedeem ClaimBid  (nft "a"<>lovelaceValueOf 100_000_000) (defaultBid $ Wallet 2)
        <> builderPayTo ( Wallet 2) (nft "a")
        <> builderPayTo (Wallet 1) (lovelaceValueOf 99_000_000)
        <> builderPayTo operator (lovelaceValueOf  1_000_000)
        <> builderSign (Wallet 2)
      ) (startInclusiveEndExclusiveRange 1900 2000),
      execFailMarketTimed "When Bidder claims, Cannot pay less fees"(
           builderRedeem ClaimBid  (nft "a"<>lovelaceValueOf 100_000_000) (defaultBid $ Wallet 2)
        <> builderPayTo ( Wallet 2) (nft "a")
        <> builderPayTo (Wallet 1) (lovelaceValueOf 99_000_000)
        <> builderPayTo operator  (lovelaceValueOf  500_000)
        <> builderPayTo (Wallet 2) (lovelaceValueOf  500_000)
        <> builderSign (Wallet 1)
      ) (startInclusiveEndExclusiveRange 1900 2000),
      execFailMarketTimed "When bidder claims,Cannot pay less to auction starter "(
           builderRedeem ClaimBid  (nft "a"<>lovelaceValueOf 100_000_000) (defaultBid $ Wallet 2)
        <> builderPayTo ( Wallet 2) (nft "a")
        <> builderPayTo (Wallet 1) (lovelaceValueOf 98_000_000)
        <> builderPayTo (Wallet 2) (lovelaceValueOf  1_000_000)
        <> builderPayTo operator (lovelaceValueOf  1_000_000)
        <> builderSign (Wallet 1)
      ) (startInclusiveEndExclusiveRange 1900 2000),
      execFailMarketTimed "When Owner claims, Cannot pay less fees"(
           builderRedeem ClaimBid  (nft "a"<>lovelaceValueOf 100_000_000) (defaultBid $ Wallet 2)
        <> builderPayTo ( Wallet 2) (nft "a")
        <> builderPayTo (Wallet 1) (lovelaceValueOf 100_00_000)
        <> builderSign (Wallet 1)
      ) (startInclusiveEndExclusiveRange 1900 2000),
      execFailMarketTimed "When Owner claims, must pay asset to auction winner"(
           builderRedeem ClaimBid  (nft "a"<>lovelaceValueOf 100_000_000) (defaultBid $ Wallet 2)
        <> builderPayTo ( Wallet 2) (nft "a")
        <> builderPayTo (Wallet 1) (lovelaceValueOf 98_000_000)
        <> builderPayTo (Wallet 2) (lovelaceValueOf  1_000_000)
        <> builderPayTo operator (lovelaceValueOf  1_000_000)
      ) (startInclusiveEndExclusiveRange 1900 2000) 

    ]

  ]

execMarketTimed :: TestName-> TestContextBuilder -> POSIXTimeRange -> TestTree
execMarketTimed name ctx range =testCase name (executeSpendContext  _marketValidator ctx range @?= True)
  where
    _marketValidator d r ctx= case fromData r of
      Just redeemer -> mkMarket defaultMarket d redeemer ctx
      _     -> False

defaultSale :: DirectSale
defaultSale= DirectSale (pubKeyHash $ walletPubKey $ Wallet 1) (Price (adaSymbol, adaToken, 100_000_000)) Primary

primarySale :: Wallet -> Price  -> DirectSale
primarySale w cost = DirectSale (pubKeyHash $ walletPubKey w) cost Primary

priceInToken :: ByteString -> Integer -> Price
priceInToken token iValue = Price (CurrencySymbol token, TokenName "", iValue)
priveInLovelace :: Integer->Price
priveInLovelace v=Price (adaSymbol, adaToken, v)

execMarket :: TestName-> TestContextBuilder -> TestTree
execMarket testName  ctx = execMarketTimed testName ctx  always

execFailMarketTimed :: TestName-> TestContextBuilder -> POSIXTimeRange -> TestTree
execFailMarketTimed name ctx range =testCase name (executeSpendContext  _marketValidator ctx range @?= False)
  where
    _marketValidator d r ctx= case fromData r of
      Just redeemer -> mkMarket defaultMarket d redeemer ctx
      _     -> False

execFailMarket :: TestName-> TestContextBuilder -> TestTree
execFailMarket testName  ctx = execFailMarketTimed testName ctx  always

defaultW1sAuction::Auction 
defaultW1sAuction=makeAuction (Wallet 1) (nft "a") (startInclusiveEndExclusiveRange 1000 2000) (priveInLovelace 100_000_000)



defaultBid :: Wallet -> Auction
defaultBid w=Auction{
    aOwner  = pubKeyHash $ walletPubKey  $ Wallet 1,
    aBidder = pubKeyHash $ walletPubKey w, -- Current Bidder
    aAssetClass=AssetClass (adaSymbol , adaToken ), -- The Bidding currency for auction.
    aMinBid = 100_000_000, -- starting Bid
    aMinIncrement =10_000_000, -- min increment  from previous auction per bid
    aDuration = startInclusiveEndExclusiveRange 1000 2000, -- Auction duration
    aValue =  nft "a"
}

startInclusiveEndExclusiveRange :: Integer -> Integer -> POSIXTimeRange 
startInclusiveEndExclusiveRange a b= Interval (LowerBound (Finite $ POSIXTime a) True ) (UpperBound (Finite $ POSIXTime  b) False)

makeAuction::Wallet->Value->POSIXTimeRange ->Price ->Auction
makeAuction wallet content range (Price (c, t, i)) =Auction {
    aOwner  = pubKeyHash $ walletPubKey  wallet,
    aBidder = pubKeyHash $ walletPubKey wallet, -- Current Bidder
    aAssetClass=AssetClass (c, t), -- The Bidding currency for auction.
    aMinBid = i, -- starting Bid
    aMinIncrement =10_000_000, -- min increment  from previous auction per bid
    aDuration = range, -- Auction duration
    aValue =  content
}



