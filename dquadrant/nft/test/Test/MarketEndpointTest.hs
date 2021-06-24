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
module Test.MarketEndpointTest
(tests)
where

import Test.TestHelper
import Control.Monad (void)
import qualified Data.Map as Map
import Data.Text ( Text )
import qualified Ledger.Ada as Ada
import Ledger.Value as Value
import Plutus.Contract.Test
import qualified Plutus.Trace.Emulator as Trace
import PlutusTx.Prelude
import Test.Tasty
import Plutus.Contract.Blockchain.MarketPlace
import Plutus.Contract.Wallet.MarketEndpoints
import Plutus.Contract.Wallet.EndpointModels
import Plutus.Contract.Wallet.MarketPlace
import Control.Lens
import Plutus.Trace.Emulator
import qualified Data.Aeson.Types as AesonTypes
import Ledger (pubKeyHash, TxId (TxId), TxOutRef, txId, TxOutTx (txOutTxTx))
import Ledger.Ada (adaSymbol,adaToken, lovelaceValueOf)
import Plutus.Trace.Emulator.ContractInstance
import Plutus.Contract.Types
import Data.Aeson (fromJSON, Result (Success, Error), toJSON, FromJSON)
import Plutus.Contract (HasEndpoint, Endpoint, type (.\/), endpoint, tell, logInfo, utxoAt)
import Control.Monad.IO.Class (liftIO)
import Prelude (putStrLn, Show (show), String, tail)
import qualified Control.Monad.Freer.Extras.Log as Extras



tests :: TestTree
tests = testGroup "directSale"
      [ canPlaceForDirectSale
      -- , canPlaceMultipleForDirectsale
      -- , canWithdrawFromMarket
      -- , whenSpendPrice'canBuyFromMarket
      -- , canBuyMultipleFromMarket
      -- , whenBought'operatorReceivesFee
      -- , whenBoughtMultiple'operatorReceivesFee
      -- , whenBought'sellerReceivesSellerShare
      -- , whenBoughtMultiple'sellerReceivesSellerShare
      ]

emCfg :: EmulatorConfig
emCfg = EmulatorConfig $ Left $ Map.fromList distribution
  where
    distribution= [
                (Wallet 1 ,adaFunds<>nft "aa" <> nft "ab" <> nft "ac"),
                (Wallet 2 ,adaFunds<>nft "ba" <> nft "bb" <> nft "bc"),
                (Wallet 3 ,adaFunds<>nft "ca" <> nft "cb" <> nft "cc"),
                (Wallet 4 ,adaFunds<>nft "da" <> nft "db" <> nft "dc"),
                (Wallet 5 ,adaFunds<>nft "ea" <> nft "eb" <> nft "ec"),
                (Wallet 6 ,adaFunds<>nft "fa" <> nft "fb" <> nft "fc")
        ]
    adaFunds :: Value
    adaFunds = Ada.lovelaceValueOf 100_000_000_000
    
defaultCheck :: String -> TracePredicate -> EmulatorTrace () -> TestTree
defaultCheck=checkPredicateOptions (defaultCheckOptions & emulatorConfig .~ emCfg)

canPlaceForDirectSale :: TestTree
canPlaceForDirectSale =
    defaultCheck
      "Can Place nfts On directsale"
      (     walletFundsChange (Wallet 1) (negNft "aa")
      .&&. walletFundsChange (Wallet 2)  (negNft "ba")
       )$
       do
          h1 <- getHandle 1
          h2 <- getHandle 2
          void $ waitNSlots 1
          callEndpoint @"sell" h1 [ sellParamLovelace (nft "aa") Primary 10_000_000 ]
          wait
          callEndpoint @"sell" h2 [ sellParamLovelace (nft "ba") Secondary 30_000_000]
          wait


-- canPlaceMultipleForDirectsale :: TestTree
-- canPlaceMultipleForDirectsale=
--   defaultCheck
--     "can Place Multiple Nfts on Single sale"
--     (   walletFundsChange  (Wallet 1) (negNft "aa" <> negNft "ab")
--       .&&. walletFundsChange (Wallet 2) (negNft "ba" <> negNft "bb")
--      )$
--     do
--       h1 <- getHandle 1
--       h2 <- getHandle 2
--       callEndpoint @"sell" h1 [sellParamLovelace (nft "aa" <> nft "ab") Primary 1_000_000
--       callEndpoint @"sell" h2 $ sellParamLovelace (nft "ba"<> nft "bb") Secondary 1000_000
--       wait
--
-- canWithdrawFromMarket :: TestTree
-- canWithdrawFromMarket=
--   defaultCheck
--     "can Withdraw from market"
--     (   walletFundsChange  (Wallet 1) (noNft "aa")
--       .&&. walletFundsChange (Wallet 2) (noNft "bb"<> lovelaceValueOf 0)
--      )$
--     do
--       h1 <- getHandle 1
--       h2 <- getHandle 2
--       callEndpoint @"sell" h1 $ sellParamLovelace (nft "aa") Primary 1000_000
--       wait
--       callEndpoint @"sell" h2 $ sellParamLovelace (nft "bb") Secondary 1000_000
--       wait
--       last <-lastUtxos h1
--       callEndpoint @"cancelSale" h1 last
--       wait
--       last<-lastUtxos h2
--       callEndpoint @"cancelSale" h2 last
--       wait

-- canChangePriceInTheMarket=False
-- whenNotOwnNft_cannotChangePriceInMarket=False

-- whenSpendPrice'canBuyFromMarket :: TestTree
-- whenSpendPrice'canBuyFromMarket=
--   defaultCheck
--     "When required Asset is Spent, Can buy item in Market"
--     (   walletFundsChange  (Wallet 1) (nft "bb"<>lovelaceValueOf (-2_000_000))
--       .&&. walletFundsChange (Wallet 3) (nft "da" <> lovelaceValueOf (-4_000_000))
--      )$
--     do
--       h1 <- getHandle 1
--       h2 <- getHandle 2
--       h3 <-getHandle 3
--       h4 <- getHandle 4
--       callEndpoint @"sell" h2 $ sellParamLovelace (nft "bb") Secondary 2_000_000
--       callEndpoint @"sell" h4 $ sellParamLovelace (nft "da") Primary 4_000_000
--       wait
--       ppValue <-lastUtxos h2
--       callEndpoint @"buy" h1 $  PurchaseParam ppValue  (valueInfoLovelace 2_000_000)
--       ppValue <- lastUtxos h4
--       callEndpoint @"buy" h3 $  PurchaseParam ppValue  (valueInfoLovelace 4_000_000)
--       wait

-- canBuyMultipleFromMarket=
--   defaultCheck
--     "Can buy Multiple items on sale in single transaction"
--     (   walletFundsChange  (Wallet 1)
--     (negNft "aa" <>nft "bb" <> nft "cc" <>nft "da" <> lovelaceValueOf (-9_000_000))
--      )$
--     do
--       h1 <- getHandle 1
--       h2 <- getHandle 2
--       h3 <-getHandle 3
--       h4 <- getHandle 4
--       callEndpoint @"sell" h1 $ sellParamLovelace (nft "aa") Primary 1_000_000
--       callEndpoint @"sell" h2 $ sellParamLovelace (nft "bb") Secondary 2_000_000
--       callEndpoint @"sell" h3 $ sellParamLovelace (nft "cc") Secondary 3_000_000
--       callEndpoint @"sell" h4 $ sellParamLovelace (nft "da") Primary 4_000_000
--       wait
--       callEndpoint @"buy" h1 [nftAssetId "bb", nftAssetId "cc", nftAssetId "da"]
--       wait

-- whenBought'operatorReceivesFee :: TestTree
-- whenBought'operatorReceivesFee=
--     defaultCheck
--     "Operator Receives fee on purchases"
--     (  walletFundsChange operator (lovelaceValueOf (4_000_000+3_000_000)))$
--     do
--       h1 <- getHandle 1
--       h2 <- getHandle 2
--       callEndpoint @"sell" h1 $ sellParamLovelace (nft "aa") Primary 200_000_000
--       callEndpoint @"sell" h2 $ sellParamLovelace (nft "bb") Secondary 100_000_000
--       wait
--       callEndpoint @"buy" h1  [nftAssetId "aa"]
--       callEndpoint @"buy" h2  [nftAssetId "bb"]
--       wait


-- whenBoughtMultiple'operatorReceivesFee :: TestTree
-- whenBoughtMultiple'operatorReceivesFee=
--     defaultCheck
--     "Operator receives all fees when buying multiple nfts in single transaction"
--     (walletFundsChange operator (lovelaceValueOf (4_000_000+3_000_000+9_000_000))) $
--     do
--       h1 <- getHandle 1
--       h2 <- getHandle 2
--       h3 <- getHandle 3
--       h4 <-getHandle 4
--       callEndpoint @"sell" h1 $ sellParamLovelace (nft "aa") Primary 200_000_000
--       callEndpoint @"sell" h2 $ sellParamLovelace (nft "ba") Secondary 100_000_000
--       callEndpoint @"sell" h3 $ sellParamLovelace  (nft "ca") Secondary 300_000_000
--       wait
--       callEndpoint @"buy" h4  [nftAssetId "aa",nftAssetId "ba",nftAssetId "ca"]
--       wait

-- whenBought'sellerReceivesSellerShare :: TestTree
-- whenBought'sellerReceivesSellerShare=
--     defaultCheck
--     "Seller receives the sell price on sale"
--     (   walletFundsChange (Wallet 1) (negNft "aa" <> lovelaceValueOf 196_000_000)
--     .&&.walletFundsChange (Wallet 2) (negNft "bb" <> lovelaceValueOf 97_000_000)
--     )$
--     do
--       h1 <- getHandle 1
--       h2 <- getHandle 2
--       h3 <- getHandle 3
--       callEndpoint @"sell" h1 $ sellParamLovelace (nft "aa") Primary 200_000_000
--       callEndpoint @"sell" h2 $ sellParamLovelace (nft "bb") Secondary 100_000_000
--       wait
--       callEndpoint @"buy" h3  [nftAssetId "aa"]
--       wait
--       callEndpoint @"buy" h3  [nftAssetId "bb"]
--       wait

-- whenBoughtMultiple'sellerReceivesSellerShare :: TestTree
-- whenBoughtMultiple'sellerReceivesSellerShare=
--     defaultCheck
--     "Seller receives sell prices of each nfts bought in single transaction"
--     (   walletFundsChange (Wallet 1) (negNft "aa"<> negNft "ab" <>
--     lovelaceValueOf 293_000_000)
--     .&&.walletFundsChange (Wallet 2) (negNft "ba" <>lovelaceValueOf 97_000_000)
--     .&&.walletFundsChange (Wallet 3)  (negNft "ca" <>lovelaceValueOf 291_000_000)
--     )$    do
--       h1 <- getHandle 1
--       h2 <- getHandle 2
--       h3 <- getHandle 3
--       h4 <-getHandle 4
--       callEndpoint @"sell" h1 $ sellParamLovelace (nft "aa") Primary 200_000_000
--       wait
--       callEndpoint @"sell" h1 $ sellParamLovelace (nft "ab") Secondary 100_000_000
--       callEndpoint @"sell" h2 $ sellParamLovelace (nft "ba") Secondary 100_000_000
--       callEndpoint @"sell" h3 $ sellParamLovelace  (nft "ca") Secondary 300_000_000
--       wait
--       callEndpoint @"buy" h4  [nftAssetId "aa",nftAssetId "ab",nftAssetId "ba",nftAssetId "ca"]
--       wait

canPlaceOnAuction=False
canUpdateAuctionParameters=False
canBidOnAuction=False
whenBid_previousBidderReceivesOldBidAmount=False
whenBid_marketReceivesTheBidAmount=False
canClaimOwnAuction=False
whenClaimAuction_marketReceivesFee=False
whenClaimAuction_ownerReceivesPayment=False