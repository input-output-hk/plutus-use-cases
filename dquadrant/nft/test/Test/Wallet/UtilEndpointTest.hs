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
module Test.Wallet.UtilEndpointTest
(tests)
where

import Test.TestHelper
import Ledger.Ada ( lovelaceValueOf )
import Ledger.Value as Value ( geq )
import Plutus.Contract.Test
import PlutusTx.Prelude hiding (Eq((==)))
import Test.Tasty
import Plutus.Contract.Wallet.EndpointModels hiding(value)
import Plutus.Trace.Emulator
import Ledger.TimeSlot (slotToPOSIXTime)
import Ledger hiding(singleton)
import Prelude (show, Eq ((==)), String)
import Data.Functor
import qualified Data.Aeson.Types as AesonTypes
import Data.Text (Text)


tests :: TestTree
tests = testGroup "Auction"
      [ canQueryOwnBalance]


utilTest :: String -> TracePredicate -> EmulatorTrace () -> TestTree
utilTest  a distributio b c n=checkPredicateOptions (defaultCheckOptions & emulatorConfig .~ ( EmulatorConfig $ Left $ Map.fromList distribution)) a b c


canQueryOwnBalance::TestTree
canQueryOwnBalance=utilTest
  "Responds lovelace Balances"
    ( ([EmulatorTimeEvent UserThreadMsg] -> Bool))
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