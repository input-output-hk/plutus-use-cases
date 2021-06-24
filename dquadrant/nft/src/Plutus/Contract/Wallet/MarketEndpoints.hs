{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
module Plutus.Contract.Wallet.MarketEndpoints
where
------------------
-- Market Endpoints.hs
------------------------

import GHC.Generics (Generic)
import Data.Aeson (ToJSON (toJSON), FromJSON)
import Data.ByteString (ByteString)
import Playground.Contract
import Ledger hiding(txOutDatum,fee,value,singleton,LowerBound,UpperBound)
import Ledger.Value
import qualified PlutusTx.Prelude as PPrelude
import Plutus.Contract.Blockchain.MarketPlace
import Plutus.Contract.Wallet.EndpointModels
import Plutus.Contract.Wallet.MarketPlace
import Plutus.Contract.Blockchain.Utils

import Data.Text (Text)
import qualified Data.Map as Map
import Plutus.Contract ( select, handleError, tell, HasEndpoint, awaitTxConfirmed, logInfo)
import Ledger.AddressMap (UtxoMap)
import qualified Plutus.Contract as Contract
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Internal as Lazy
import qualified Ledger.Time              as Time
import Data.Functor ((<&>), void)
import Ledger.Interval
import Plutus.Contract.Wallet.Utils

type MarketSchema =
        Endpoint "sell" [SellParams]
        .\/ Endpoint "buy"  PurchaseParam
        .\/ Endpoint "onsale" String -- ignores the string
        .\/ Endpoint "ownOnSale" String -- ignores the string
        .\/ Endpoint "onSaleOfPk" PubKeyHash
        .\/ Endpoint "cancelSale" [TxOutRef]
        .\/ Endpoint "startAuction" [AuctionParam]
        .\/ Endpoint "bid"  BidParam
        .\/ Endpoint "claim"  ClaimParam

-- mkSchemaDefinitions ''MarketSchema


-- Not hardcoding it to MarketSchema gives us 
-- flexibility to merge this endpoint with other endpoints
-- Note that this endpoint dies after first request. 
-- use (marketEndpoints m >> marketEndpoints m) to serve requests forever.
marketEndpoints :: (
    HasEndpoint "sell" [SellParams] s,
    HasEndpoint "buy"  PurchaseParam  s,
    HasEndpoint "onsale" String s,
    HasEndpoint "ownOnSale" String s,
    HasEndpoint "onSaleOfPk" PubKeyHash s,
    HasEndpoint "cancelSale" [TxOutRef] s,
    HasEndpoint "startAuction" [AuctionParam] s,
    HasEndpoint "bid" BidParam s,
    HasEndpoint "claim" ClaimParam s
    ) => Market -> Contract [AesonTypes.Value] s Text  ()
marketEndpoints market =  handleError handler (void selections)
  where
    selections  =      sellEp market
              `select` buyEp market
              `select` onSaleEp market
              `select` ownDirectSalesEp market
              `select` directSalesOfPkEp market
              `select` cancelSaleEp market
              `select` startAuctionEp market
              `select` bidEp market
              `select` claimEp market

    handler :: Show a => a -> Contract w s e ()
    handler e = do
        Contract.logError $ show e


sellEp :: HasEndpoint "sell" [SellParams] s =>Market -> Contract [AesonTypes.Value] s Text AesonTypes.Value
sellEp market =do
    logInfo @String "Enabled Sell Endpoint"
    sps <-(endpoint @"sell")
    logInfo @String $ "Selling items" ++ show sps
    let aggregateValue=valueInfosToValue (spItems$ head sps)
    sell market (spSaleType $head sps) aggregateValue (valueInfoToPrice $ spCost$ head sps)
      >>=confirmAndTell

cancelSaleEp :: HasEndpoint "cancelSale" [TxOutRef] s => Market -> Contract [AesonTypes.Value] s Text AesonTypes.Value
cancelSaleEp m =(endpoint @"cancelSale">>= cancelUtxoSale  m) >>= confirmAndTell

buyEp :: (HasEndpoint "buy" PurchaseParam s) => Market -> Contract [AesonTypes.Value] s Text AesonTypes.Value
buyEp market= do
  PurchaseParam{ppItems,ppValue} <-(endpoint @"buy")
  findMarketUtxos market ppItems >>= buyDirectSaleUtxos market >>= confirmAndTell

onSaleEp :: HasEndpoint "onsale" String s =>
    Market -> Contract [AesonTypes.Value] s Text AesonTypes.Value
onSaleEp market =
        (endpoint @"onsale"
        >> directSalesInMarket market)
        >>= doReturn . map (directSaleToResponse market)

directSalesOfPkEp :: (HasEndpoint "onSaleOfPk" PubKeyHash s,AsContractError e) =>
             Market-> Contract [AesonTypes.Value ] s e AesonTypes.Value
directSalesOfPkEp m =
  endpoint @"onSaleOfPk"
     >>= directSalesOfPk m
     >>= doReturn . map (directSaleToResponse m)

ownDirectSalesEp :: (HasEndpoint "ownOnSale" String s,AsContractError e) =>
             Market-> Contract [AesonTypes.Value ] s e AesonTypes.Value
ownDirectSalesEp m =
  endpoint @"ownOnSale"
     >> ownPubKey
     >>= directSalesOfPk m . pubKeyHash
     >>= doReturn . map (directSaleToResponse m)

-- updateSaleEp::  HasEndpoint "claim" [AssetId] s => Market -> Contract [AesonTypes.Value] s Text AesonTypes.Value
-- updateSaleEp market=do
--     Contract.waitNSlots 1
--     pure $ toJSON ()

startAuctionEp::  HasEndpoint "startAuction" [AuctionParam] s => Market -> Contract [AesonTypes.Value] s Text AesonTypes.Value
startAuctionEp market=do
    aps <- endpoint @"startAuction"
    ownPkh <-ownPubKey <&> pubKeyHash
    let as= map (aParamToAuction ownPkh) aps
    submitAuction market as >>= confirmAndTell
  
-- TODO Investigate : Using this prime version  somehow makes all other endpoints go away. 
startAuctionEp'::  HasEndpoint "startAuction" [AuctionParam] s => Market -> Contract [AesonTypes.Value] s Text AesonTypes.Value
startAuctionEp' market =do 
  ownPkh <- ownPubKey <&> pubKeyHash
  endpoint @"startAuction" 
    <&> map (aParamToAuction ownPkh)
    >>= submitAuction market
    >>= confirmAndTell

bidEp::  HasEndpoint "bid" BidParam  s => Market -> Contract [AesonTypes.Value] s Text AesonTypes.Value
bidEp market=do
  (BidParam ref bid) <- endpoint @"bid"
  resolved <-parseMarketUtxo  market ref
  bidAuctionUtxo market resolved (valueInfosToValue bid)  >>= confirmAndTell

claimEp::  HasEndpoint "claim" ClaimParam  s => Market -> Contract [AesonTypes.Value] s Text AesonTypes.Value
claimEp market=do
  (ClaimParam refs acceptError) <- endpoint @"claim"
  parsed <-(if acceptError then parseMarketUtxosNoError else parseMarketUtxos) market refs
  claimAuctionUtxos market parsed >>= confirmAndTell

confirmAndTell ::  AsContractError e =>  Tx -> Contract [AesonTypes.Value] s e AesonTypes.Value
confirmAndTell  tx = do
  let id=txId tx
  awaitTxConfirmed id
  doReturn id

doReturn :: ToJSON a => a -> Contract [AesonTypes.Value] s e AesonTypes.Value
doReturn x =do
  tell [toJSON x]
  pure $ toJSON x