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

import PlutusTx (fromData, FromData (fromBuiltinData))
import Ledger
import PlutusTx.Prelude
import Plutus.Contract.Test
import Ledger.Ada ( adaSymbol, adaToken, lovelaceValueOf )
import Prelude (IO)


main :: IO ()
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
      execMarket "Can buy an item  having multiple parties" (
            builderRedeem
                  Buy (nft "a")
                  (primarySale (Wallet 1) [(Wallet 2,percent 30),(Wallet 3, percent 20)]
                  (priceInLovelace 100_000_000))
        <>  builderSpend (Wallet 4) (lovelaceValueOf 100_000_000)
        <> builderPayLovelaceTo operator 2_000_000     -- Fee (2% of 100_000_000). Remaining 98_000_000 will be distributed to seller parties
        <> builderPayLovelaceTo (Wallet 2) 29_400_000  -- 30 % of 98_000_000
        <> builderPayLovelaceTo (Wallet 3) 19_600_000  -- 20 % of 98_000_000
        <> builderPayLovelaceTo (Wallet 1) 49_000_000  -- 50% of 98_000_000
      ),
      execMarket "Can buy and gift to another wallet" (
            builderRedeem Buy (nft "a") defaultSale
        <>  builderSpend (Wallet 2) (lovelaceValueOf 100_000_000)
        <>  builderPayTo (Wallet 1) (lovelaceValueOf 98_000_000)
        <>  builderPayTo (Wallet 3) (nft "a")
        <>  builderPayTo operator (lovelaceValueOf 2_000_000)
      ),
      execMarket "Can buy multiple items " (
            builderRedeem Buy (nft "a") (primarySale (Wallet 1) [] (priceInLovelace 100_000_000))
        <>  builderRedeem Buy (nft "b") (primarySale (Wallet 2) [] (priceInLovelace 200_000_000))
        <>  builderRedeem Buy (nft "c") (primarySale (Wallet 3) [] (priceInLovelace 300_000_000))
        <>  builderSpend (Wallet 4) (lovelaceValueOf 600_000_000)
        <>  builderPayTo (Wallet 1) (lovelaceValueOf 98_000_000)
        <>  builderPayTo (Wallet 2) (lovelaceValueOf 196_000_000)
        <>  builderPayTo (Wallet 3) (lovelaceValueOf 294_000_000)
        <>  builderPayTo (Wallet 2) (nft "a"<>nft"b"<>nft"c")
        <>  builderPayTo operator (lovelaceValueOf 12_000_000)
      ),
      execMarket "Can buy multiple items each having multiple parties " (
            builderRedeem
              Buy (nft "a")
              (primarySale (Wallet 1) [(Wallet 2,percent 30),(Wallet 3, percent 20)]
              (priceInLovelace 100_000_000))
        <>  builderRedeem
              Buy (nft "b")
              (primarySale (Wallet 2) [(Wallet 3,percent 30),(Wallet 1, percent 20)]
              (priceInLovelace 100_000_000))
        <>  builderRedeem
              Buy (nft "c")
              (primarySale (Wallet 3) [(Wallet 1,percent 30),(Wallet 2, percent 20)]
              (priceInLovelace 100_000_000))
        <>  builderSpend (Wallet 4) (lovelaceValueOf 300_000_000)
        <>  builderPayTo (Wallet 4) (nft "a"<>nft"b"<>nft"c")
        <>  builderPayLovelaceTo (Wallet 1)  98_000_000
        <>  builderPayLovelaceTo (Wallet 2)  98_000_000
        <>  builderPayLovelaceTo (Wallet 3)  98_000_000
        <>  builderPayLovelaceTo operator    6_000_000  -- 2ada from each nft
      ),

      execFailMarket "Cannot Pay less to seller" (
            builderRedeem Buy (nft "a") defaultSale
        <>  builderSpend (Wallet 2) (lovelaceValueOf 99_000_000)
        <>  builderPayTo (Wallet 1) (lovelaceValueOf 97_000_000)
        <>  builderPayTo (Wallet 2) (nft "a")
        <>  builderPayTo operator (lovelaceValueOf 2_000_000)
      ),
      execFailMarket "Cannot Pay less to seller parties"(
             builderRedeem
                  Buy (nft "a")
                  (primarySale (Wallet 1) [(Wallet 2,percent 30),(Wallet 3, percent 20)]
                  (priceInLovelace 100_000_000))
        <>  builderSpend (Wallet 4) (lovelaceValueOf 99_000_000)
        <> builderPayLovelaceTo operator 2_000_000     -- Fee (2% of 100_000_000). Remaining 98_000_000 will be distributed to seller parties
        <> builderPayLovelaceTo (Wallet 2) 28_400_000  -- 1 ada less than what must be paid
        <> builderPayLovelaceTo (Wallet 3) 19_600_000  -- 20 % of 98_000_000
        <> builderPayLovelaceTo (Wallet 1) 49_000_000  -- 50% of 98_000_000
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
            builderRedeem Buy (nft "a") (primarySale (Wallet 1) [] (priceInLovelace 100_000_000))
        <>  builderRedeem Buy (nft "a") (primarySale (Wallet 1) [] (priceInLovelace 100_000_000))
        <>  builderSpend (Wallet 2) (lovelaceValueOf 100_000_000)
        <>  builderPayTo (Wallet 1) (lovelaceValueOf 98_000_000)
        <>  builderPayTo (Wallet 2)  (cardanoToken "a" 2)
        <>  builderPayTo operator (lovelaceValueOf 2_000_000)
      ),
      execFailMarket "Cannot double satisfy payment to seller" (
            builderRedeem Buy (nft "a") (primarySale (Wallet 1) [] (priceInLovelace 100_000_000))
        <>  builderRedeem Buy (nft "b") (primarySale (Wallet 1) [] (priceInLovelace 100_000_000))
        <>  builderSpend (Wallet 2) (lovelaceValueOf 102_000_000)
        <>  builderPayTo (Wallet 1) (lovelaceValueOf 98_000_000)
        <>  builderPayTo (Wallet 2)  (nft "a"<>nft "b")
        <>  builderPayTo operator (lovelaceValueOf 4_000_000)
      ),
      execFailMarket "Cannot double satisfy marketFee" (
            builderRedeem Buy (nft "a") (primarySale (Wallet 1) [] (priceInLovelace 100_000_000))
        <>  builderRedeem Buy (nft "b") (primarySale (Wallet 3) [] (priceInLovelace 100_000_000))
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
        builderRedeem Withdraw (nft "a") (primarySale (Wallet 2) [](priceInLovelace 100_000_000))
        <>  builderRedeem Withdraw (nft "b") (primarySale (Wallet 2) [](priceInLovelace 200_000_000))
        <>  builderPayTo (Wallet 2) (nft "a" <> nft "b")
        <> builderSign (Wallet 2)
      ),
      execFailMarket "Cannot withdraw without seller signature" (
        builderRedeem Withdraw (nft "a") (primarySale (Wallet 1) [] (priceInLovelace 100_000_000))
        <>  builderPayTo (Wallet 1) (nft "a")
        <> builderSign (Wallet 2)
      ),
      execFailMarket "Cannot withdraw my mixing with own item" (
            builderRedeem Withdraw (nft  "a") (primarySale (Wallet 2) [](priceInLovelace 100_000_000))
        <>  builderRedeem Withdraw (nft "a") (primarySale (Wallet 1) [](priceInLovelace 100_000_000))
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
      execMarketTimed "Can withdraw during Auction interval boundary if no bid" (
            builderRedeem Withdraw (nft "a") defaultW1sAuction
        <>  builderPayTo (Wallet 1) (nft "a")
        <>  builderSign (Wallet 1)
      ) (startInclusiveEndExclusiveRange 1000 2000),
      execMarketTimed "Can withdraw during Auction interval if no bid" (
            builderRedeem Withdraw (nft "a") defaultW1sAuction
        <>  builderPayTo (Wallet 1) (nft "a")
        <>  builderSign (Wallet 1)
      ) (startInclusiveEndExclusiveRange 1400 1600),
      execFailMarketTimed "Can withdraw during Auctions with bidder's signature" (
            builderRedeem Withdraw (nft "a") (defaultBid $ Wallet 2)
        <>  builderPayTo (Wallet 2) (nft "a")
        <>  builderSign (Wallet 2)
      ) (startInclusiveEndExclusiveRange 1400 1600),
      execMarketTimed "Can withdraw at Auction interval boundary if there's bid" (
            builderRedeem Withdraw (nft "a") defaultW1sAuction
        <>  builderPayTo (Wallet 1) (nft "a")
        <>  builderSign (Wallet 1)
      ) (startInclusiveEndExclusiveRange 1000 2000),
      execMarketTimed "Cannot withdraw during Auction if there's bid" (
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
      execMarketTimed "Can Claim completed auction"(
           builderRedeem ClaimBid  (nft "a"<>lovelaceValueOf 100_000_000) (defaultBid $ Wallet 2)
        <> builderPayTo ( Wallet 2) (nft "a")
        <> builderPayTo (Wallet 1) (lovelaceValueOf 99_000_000)
        <> builderPayTo operator (lovelaceValueOf  1_000_000)
      ) (startInclusiveEndExclusiveRange 2000 25000),
      execMarketTimed "Can Claim completed auction having parties"(
           builderRedeem ClaimBid  (nft "a"<>lovelaceValueOf 100_000_000)
              (defaultBidWithParties  (Wallet 4) [(Wallet 2,percent 25),(Wallet 3,percent 30)])
        <> builderPayTo ( Wallet 4) (nft "a")
        <> builderPayLovelaceTo operator 1_000_000    -- 1% of 100_000_000 as fee
        <> builderPayLovelaceTo (Wallet 1) 44_550_000 -- 45% of 99_000_0000
        <> builderPayLovelaceTo (Wallet 2) 24_750_000 -- 25% of 99_000_000
        <> builderPayLovelaceTo (Wallet 3) 29_700_000 -- 30% of 99_000_000
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
      execFailMarketTimed "When Bidder claims auction having parties, cannot payless to parties"(
         builderRedeem ClaimBid  (nft "a"<>lovelaceValueOf 100_000_000)
              (defaultBidWithParties  (Wallet 4) [(Wallet 2,percent 25),(Wallet 3,percent 30)])
        <> builderPayTo ( Wallet 2) (nft "a")
        <> builderPayLovelaceTo operator 1_000_000    -- 1% of 100_000_000 as fee
        <> builderPayLovelaceTo (Wallet 1) 99_000_000
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
        <> builderSign (Wallet 1)
      ) (startInclusiveEndExclusiveRange 1900 2000)
      ,
      execFailMarketTimed "When Owner claims auction having parties, must pay to the parties"(
         builderRedeem ClaimBid  (nft "a"<>lovelaceValueOf 100_000_000)
              (defaultBidWithParties  (Wallet 4) [(Wallet 2,percent 25),(Wallet 3,percent 30)])
        <> builderPayTo ( Wallet 2) (nft "a")
        <> builderPayLovelaceTo operator 1_000_000    -- 1% of 100_000_000 as fee
        <> builderPayLovelaceTo (Wallet 1) 99_000_000
        <> builderSign (Wallet 1)
      ) (startInclusiveEndExclusiveRange 1900 2000)

    ]

  ]

execMarketTimed :: TestName-> TestContextBuilder -> POSIXTimeRange -> TestTree
execMarketTimed name ctx range =testCase name (executeSpendContext  _marketValidator ctx range @?= True)
  where
    _marketValidator d r ctx= case fromBuiltinData  r of
      Just redeemer -> mkMarket defaultMarket d redeemer ctx
      _     -> False

defaultSale :: DirectSale
defaultSale= DirectSale{
    dsSeller=pubKeyHash $ walletPubKey $ Wallet 1,
    dsParties=[],
    dsAsset=AssetClass (adaSymbol,adaToken),
    dsCost=100_000_000,
    dsType=Primary
}
primarySale :: Wallet ->[(Wallet,Percent )] -> Price  -> DirectSale
primarySale w parties (Price(c,t,v)) = DirectSale{
    dsSeller=pubKeyHash $ walletPubKey w,
    dsParties=map (\(w,p)->(pubKeyHash $ walletPubKey w,p)) parties,
    dsAsset=AssetClass (c,t),
    dsCost=v,
    dsType=Primary
}
priceInToken :: BuiltinByteString  -> Integer -> Price
priceInToken token iValue = Price (CurrencySymbol token, TokenName "", iValue)
priceInLovelace :: Integer->Price
priceInLovelace v=Price (adaSymbol, adaToken, v)

execMarket :: TestName-> TestContextBuilder -> TestTree
execMarket testName  ctx = execMarketTimed testName ctx  always

execFailMarketTimed :: TestName-> TestContextBuilder -> POSIXTimeRange -> TestTree
execFailMarketTimed name ctx range =testCase name (executeSpendContext  _marketValidator ctx range @?= False)
  where
    _marketValidator d r ctx= case fromBuiltinData  r of
      Just redeemer -> mkMarket defaultMarket d redeemer ctx
      _     -> False

execFailMarket :: TestName-> TestContextBuilder -> TestTree
execFailMarket testName  ctx = execFailMarketTimed testName ctx  always

defaultW1sAuction::Auction
defaultW1sAuction=makeAuction (Wallet 1) (nft "a") (startInclusiveEndExclusiveRange 1000 2000) (priceInLovelace 100_000_000)



defaultBid :: Wallet -> Auction
defaultBid w=defaultBidWithParties w []

defaultBidWithParties:: Wallet -> [(Wallet,Percent)]-> Auction
defaultBidWithParties w parties=Auction{
    aParties =map (\(w,p)-> (pubKeyHash $ walletPubKey w,p)) parties,
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
    aParties =[],
    aOwner  = pubKeyHash $ walletPubKey  wallet,
    aBidder = pubKeyHash $ walletPubKey wallet, -- Current Bidder
    aAssetClass=AssetClass (c, t), -- The Bidding currency for auction.
    aMinBid = i, -- starting Bid
    aMinIncrement =10_000_000, -- min increment  from previous auction per bid
    aDuration = range, -- Auction duration
    aValue =  content
}



