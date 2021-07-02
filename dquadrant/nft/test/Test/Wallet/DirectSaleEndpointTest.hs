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
module Test.Wallet.DirectSaleEndpointTest
(tests)
where

import Test.TestHelper
import Control.Monad (void)
import Ledger.Ada ( lovelaceValueOf )
import Plutus.Contract.Test
import PlutusTx.Prelude
import Test.Tasty
import Plutus.Contract.Blockchain.MarketPlace
import Plutus.Contract.Wallet.EndpointModels
import Control.Lens
import Plutus.Trace.Emulator
import qualified Data.Aeson as AesonTypes
import Data.Text(Text)
import Ledger (TxOutRef, pubKeyHash, PubKeyHash (getPubKeyHash))
import Prelude (show)


-- Tests in this file test wallet code and not the validator itself
-- The tests will pass with the correct validator but it will also pass if
-- you replace the market validator with a dummyOne like this.
--
-- marketValidator :: Market -> Validator
-- marketValidator market= mkValidatorScript $$(PlutusTx.compile [||a ||])
--     where
--         a _ _ _=()


tests :: TestTree
tests = testGroup "DirectSale"
      [ canPlaceForDirectSale
      , canQueryOwnAndOthersAuction
      , canPlaceCombosOnSale
      , canWithdrawFromMarket
      , canWithdrawMultipleFromMarket
      , whenSpendPrice'canBuyFromMarket
      , canBuyMultipleFromMarket
      , whenBought'operatorReceivesFee
      , whenBoughtMultiple'operatorReceivesFee
      , whenBought'sellerReceivesSellerShare
      , whenBoughtMultiple'sellerReceivesSellerShare
      ]

canPlaceForDirectSale :: TestTree
canPlaceForDirectSale =
    defaultCheck
      "Can Place nfts On directsale"
      (     walletFundsChange (Wallet 1) (negNft "aa")
      .&&. walletFundsChange (Wallet 2)  (negNft "ba")
      .&&. lockedByMarket ( nft "aa" <> nft "ba")

       )$
       do
          h1 <- getHandle 1
          h2 <- getHandle 2

          void $ waitNSlots 1
          callEndpoint @"sell" h1 [ sellParamLovelace (nft "aa") Primary 10_000_000 ]
          callEndpoint @"sell" h2 [ sellParamLovelace (nft "ba") Secondary 30_000_000]
          wait
          sales<-callListDirectSale h1
          assertTrue "Expected two items in marketsale"  $ (length sales)==2


canQueryOwnAndOthersAuction ::TestTree
canQueryOwnAndOthersAuction = defaultCheck
  "Can List Own Auction in Market"
  assertNoFailedTransactions
  $ do
    h1<-getHandle 1
    h2<- getHandle 2

    callEndpoint @"sell" h1 [ sellParamLovelace (nft "aa") Primary 10_000_000 ]
    callEndpoint @"sell" h2 [ sellParamLovelace (nft "ba") Secondary 30_000_000]
    u1 <-waitForLastUtxos h1 <&> head
    u2 <-lastUtxos h2 <&> head
    callListOwn h1 >>= expectSingleResponse u1
    callListOfWallet 1 h2 >>= expectSingleResponse u1

    callListOwn h2 >>=expectSingleResponse u2
    callListOfWallet 2 h1 >>= expectSingleResponse u2

canPlaceCombosOnSale :: TestTree
canPlaceCombosOnSale=
  defaultCheck
    "Can Place Combo Nfts in Single sale"
    (   walletFundsChange  (Wallet 1) (negNft "aa" <> negNft "ab")
      .&&. walletFundsChange (Wallet 2) (negNft "ba" <> negNft "bb")
     )$
    do
      h1 <- getHandle 1
      h2 <- getHandle 2
      callEndpoint @"sell" h1 [sellParamLovelace (nft "aa" <> nft "ab") Primary 1_000_000]
      callEndpoint @"sell" h2 [ sellParamLovelace (nft "ba"<> nft "bb") Secondary 1000_000]
      wait

canWithdrawFromMarket :: TestTree
canWithdrawFromMarket=
  defaultCheck
    "Can Withdraw from market"
    (   walletFundsChange  (Wallet 1) (noNft "aa")
      .&&. walletFundsChange (Wallet 2) (noNft "bb")
     )$
    do
      h1 <- getHandle 1
      h2 <- getHandle 2
      callEndpoint @"sell" h1  [sellParamLovelace (nft "aa") Primary 1000_000]
      callEndpoint @"sell" h2 [sellParamLovelace (nft "bb") Secondary 1000_000]
      wait
      lastUtxos h1 >>= callEndpoint @"withdraw" h1
      lastUtxos h2 >>= callEndpoint @"withdraw" h2
      wait

canWithdrawMultipleFromMarket :: TestTree
canWithdrawMultipleFromMarket=
  defaultCheck
    "Can Withdraw  Multiple direct sales from market"
    (   walletFundsChange  (Wallet 1) (noNft "aa"<> noNft "bb")
     )$
    do
      h1 <- getHandle 1
      callEndpoint @"sell" h1  [sellParamLovelace (nft "aa") Primary 1000_000]
      u1 <- lastUtxos h1
      callEndpoint @"sell" h1 [sellParamLovelace (nft "ab") Secondary 1000_000]
      wait
      u2 <- lastUtxos h1
      void $ callEndpoint @"withdraw" h1 (u1 ++ u2)
      wait

whenSpendPrice'canBuyFromMarket :: TestTree
whenSpendPrice'canBuyFromMarket=
  defaultCheck
    "When required Asset is Spent, Can buy item in Market"
    (   walletFundsChange  (Wallet 1) (nft "bb"<>lovelaceValueOf (-2_000_000))
      .&&. walletFundsChange (Wallet 3) (nft "da" <> lovelaceValueOf (-4_000_000))
     )$
    do
      h1 <- getHandle 1
      h2 <- getHandle 2
      h3 <-getHandle 3
      h4 <- getHandle 4
      callEndpoint @"sell" h2 [sellParamLovelace (nft "bb") Secondary 2_000_000]
      callEndpoint @"sell" h4 [sellParamLovelace (nft "da") Primary 4_000_000]
      wait
      lastUtxos h2 <&> toParam 2_000_000 >>= callEndpoint @"buy" h1
      lastUtxos h4 <&> toParam 4_000_000 >>= callEndpoint @"buy" h3
      wait
  where
    toParam v = PurchaseParam (valueInfoLovelace v)

canBuyMultipleFromMarket :: TestTree
canBuyMultipleFromMarket=
  defaultCheck
    "Can buy Multiple items in single transaction"
    (   walletFundsChange  (Wallet 1)
    (nft "bb" <> nft "cc" <>nft "da" <> lovelaceValueOf (-9_000_000))
     )$
    do
      h1 <- getHandle 1
      h2 <- getHandle 2
      h3 <-getHandle 3
      h4 <- getHandle 4
      callEndpoint @"sell" h2 [sellParamLovelace (nft "bb") Secondary 2_000_000]
      callEndpoint @"sell" h3 [sellParamLovelace (nft "cc") Secondary 3_000_000]
      callEndpoint @"sell" h4 [sellParamLovelace (nft "da") Primary 4_000_000]
      wait
      us <-  callListDirectSale h1 <&> map reference
      callEndpoint @"buy" h1 $ PurchaseParam (valueInfoLovelace 9_000_000) us
      wait


whenBought'operatorReceivesFee :: TestTree
whenBought'operatorReceivesFee=
    defaultCheck
    "Operator Receives fee on purchases"
    (  walletFundsChange operator (lovelaceValueOf (4_000_000+3_000_000)))$
    do
      h1 <- getHandle 1
      h2 <- getHandle 2
      callEndpoint @"sell" h1 [sellParamLovelace (nft "aa") Primary 200_000_000]
      callEndpoint @"sell" h2 [sellParamLovelace (nft "bb") Secondary 100_000_000]
      wait
      lastUtxos h1 <&> toParam 100_000_000 >>=callEndpoint @"buy" h1
      lastUtxos h2 <&> toParam 200_000_000 >>=callEndpoint @"buy" h2
      wait
    where
    toParam v = PurchaseParam (valueInfoLovelace v)


whenBoughtMultiple'operatorReceivesFee :: TestTree
whenBoughtMultiple'operatorReceivesFee=
    defaultCheck
    "Operator receives all fees when buying multiple nfts in single transaction"
    (walletFundsChange operator (lovelaceValueOf (4_000_000+3_000_000+9_000_000))) $
    do
      h1 <- getHandle 1
      h2 <- getHandle 2
      h3 <- getHandle 3
      h4 <-getHandle 4
      callEndpoint @"sell" h1 [sellParamLovelace (nft "aa") Primary 200_000_000]
      callEndpoint @"sell" h2 [sellParamLovelace (nft "ba") Secondary 100_000_000]
      callEndpoint @"sell" h3 [sellParamLovelace  (nft "ca") Secondary 300_000_000]
      wait
      utxos <- callListDirectSale h4 <&> map reference
      callEndpoint @"buy" h4 $ PurchaseParam (valueInfoLovelace 600_000_000) utxos
      wait


whenBought'sellerReceivesSellerShare :: TestTree
whenBought'sellerReceivesSellerShare=
    defaultCheck
    "Seller receives the sell price on sale"
    (   walletFundsChange (Wallet 1) (negNft "aa" <> lovelaceValueOf 196_000_000)
    .&&.walletFundsChange (Wallet 2) (negNft "bb" <> lovelaceValueOf 97_000_000)
    )$
    do
      h1 <- getHandle 1
      h2 <- getHandle 2
      h3 <- getHandle 3
      callEndpoint @"sell" h1  [sellParamLovelace (nft "aa") Primary 200_000_000]
      callEndpoint @"sell" h2  [sellParamLovelace (nft "bb") Secondary 100_000_000]
      wait
      lastUtxos h1 <&> toParam 200_000_000 >>=callEndpoint @"buy" h3
      lastUtxos h2 <&> toParam 100_000_000 >>=callEndpoint @"buy" h3
      wait
  where
    toParam v = PurchaseParam (valueInfoLovelace v)

whenBoughtMultiple'sellerReceivesSellerShare :: TestTree
whenBoughtMultiple'sellerReceivesSellerShare=
    defaultCheck
    "Seller receives sell prices of each nfts bought in single transaction"
    (   walletFundsChange (Wallet 1) (negNft "aa"<> negNft "ab" <>
    lovelaceValueOf 293_000_000)
    .&&.walletFundsChange (Wallet 2) (negNft "ba" <>lovelaceValueOf 97_000_000)
    .&&.walletFundsChange (Wallet 3)  (negNft "ca" <>lovelaceValueOf 291_000_000)
    )$    do
      h1 <- getHandle 1
      h2 <- getHandle 2
      h3 <- getHandle 3
      h4 <-getHandle 4
      callEndpoint @"sell" h1 [sellParamLovelace (nft "aa") Primary 200_000_000]
      wait
      callEndpoint @"sell" h1 [sellParamLovelace (nft "ab") Secondary 100_000_000]
      callEndpoint @"sell" h2 [sellParamLovelace (nft "ba") Secondary 100_000_000]
      callEndpoint @"sell" h3 [sellParamLovelace  (nft "ca") Secondary 300_000_000]
      wait
      utxos <- callListDirectSale h4 <&> map reference
      callEndpoint @"buy" h4 $ PurchaseParam (valueInfoLovelace 700_000_000) utxos
      wait



callListDirectSale::ContractHandle [AesonTypes.Value] TestSchema Text-> EmulatorTrace [NftsOnSaleResponse]
callListDirectSale h=do
  callEndpoint @"list" h $ ListMarketRequest MtDirectSale Nothing (Just False)
  wait
  lastResult h

callListOwn::ContractHandle [AesonTypes.Value] TestSchema Text-> EmulatorTrace [NftsOnSaleResponse]
callListOwn h=do
  callEndpoint @"list" h $ ListMarketRequest MtDirectSale Nothing (Just True)
  wait
  lastResult h

callListOfWallet:: Integer -> ContractHandle [AesonTypes.Value] TestSchema Text-> EmulatorTrace [NftsOnSaleResponse]
callListOfWallet w h=do
  callEndpoint @"list" h $ ListMarketRequest MtDirectSale (Just $ getPubKeyHash $ pubKeyHash $ walletPubKey $ Wallet w) (Just False)
  wait
  lastResult h

expectSingleResponse:: TxOutRef  -> [NftsOnSaleResponse]  -> EmulatorTrace ()
expectSingleResponse utxo nfts  =case nfts of
  [a] -> assertTrue "h1 Auction response mismatch" (reference  a == utxo)
  _   ->  throw $ "Expected single response with auction got: " ++ show nfts