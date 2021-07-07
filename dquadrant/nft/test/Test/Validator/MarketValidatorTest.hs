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
tests = testGroup "Market Validator Unit Test" [
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
            builderRedeem Buy (nft "a") (primarySale (Wallet 1) (priceInAda 100_000_000))
        <>  builderRedeem Buy (nft "b") (primarySale (Wallet 2) (priceInAda 200_000_000))
        <>  builderRedeem Buy (nft "c") (primarySale (Wallet 3) (priceInAda 300_000_000))
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
            builderRedeem Buy (nft "a") (primarySale (Wallet 1) (priceInAda 100_000_000))
        <>  builderRedeem Buy (nft "a") (primarySale (Wallet 1) (priceInAda 100_000_000))
        <>  builderSpend (Wallet 2) (lovelaceValueOf 100_000_000)
        <>  builderPayTo (Wallet 1) (lovelaceValueOf 98_000_000)
        <>  builderPayTo (Wallet 2)  (cardanoToken "a" 2)
        <>  builderPayTo operator (lovelaceValueOf 2_000_000)
      ),
      execFailMarket "Cannot double satisfy payment to seller" (
            builderRedeem Buy (nft "a") (primarySale (Wallet 1) (priceInAda 100_000_000))
        <>  builderRedeem Buy (nft "b") (primarySale (Wallet 1) (priceInAda 100_000_000))
        <>  builderSpend (Wallet 2) (lovelaceValueOf 102_000_000)
        <>  builderPayTo (Wallet 1) (lovelaceValueOf 98_000_000)
        <>  builderPayTo (Wallet 2)  (nft "a"<>nft "b")
        <>  builderPayTo operator (lovelaceValueOf 4_000_000)
      ),
      execFailMarket "Cannot double satisfy marketFee" (
            builderRedeem Buy (nft "a") (primarySale (Wallet 1) (priceInAda 100_000_000))
        <>  builderRedeem Buy (nft "b") (primarySale (Wallet 3) (priceInAda 100_000_000))
        <>  builderSpend (Wallet 2) (lovelaceValueOf 198_000_000)
        <>  builderPayTo (Wallet 1) (lovelaceValueOf  98_000_000)
        <>  builderPayTo (Wallet 3) (lovelaceValueOf  98_000_000)
        <>  builderPayTo (Wallet 2)  (nft "a"<>nft "b")
        <>  builderPayTo operator (lovelaceValueOf 2_000_000)
      )
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
priceInAda :: Integer->Price
priceInAda v=Price (adaSymbol, adaToken, v)

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


