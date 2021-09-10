{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Plutus.Contract.Blockchain.MarketPlace hiding(MarketType(..))
import Plutus.Contract.Wallet.EndpointModels
import Plutus.Contract.Wallet.MarketPlace
import Plutus.Contract.Blockchain.Utils

import Data.Text (Text)
import qualified Data.Map as Map
import Plutus.Contract ( select, handleError, tell, HasEndpoint, awaitTxConfirmed, logInfo, throwError, Promise)
import Ledger.AddressMap (UtxoMap)
import qualified Plutus.Contract as Contract
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Internal as Lazy
import qualified Ledger.Time              as Time
import Data.Functor ((<&>), void)
import Ledger.Interval
import Plutus.Contract.Wallet.Utils ( throwNoUtxo )
import Control.Lens
import Control.Monad

type MarketSchema =
        Endpoint "sell" [SellParams]
        .\/ Endpoint "buy"  PurchaseParam
        .\/ Endpoint "list" ListMarketRequest 
        .\/ Endpoint "cancelSale" [TxOutRef]
        .\/ Endpoint "startAuction" [AuctionParam]
        .\/ Endpoint "bid"  BidParam
        .\/ Endpoint "claim"  ClaimParam
        .\/ Endpoint "withdraw" [TxOutRef]

-- mkSchemaDefinitions ''MarketSchema


-- Not hardcoding it to MarketSchema gives us 
-- flexibility to merge this endpoint with other endpoints
-- Note that this endpoint dies after first request. 
-- use (marketEndpoints m >> marketEndpoints m) to serve requests forever.
marketEndpoints :: (
    HasEndpoint "sell" [SellParams] s,
    HasEndpoint "buy"  PurchaseParam  s,
    HasEndpoint "list" ListMarketRequest s,
    HasEndpoint "startAuction" [AuctionParam] s,
    HasEndpoint "bid" BidParam s,
    HasEndpoint "claim" ClaimParam s,
    HasEndpoint "withdraw" [TxOutRef] s
    ) => Market -> Promise [AesonTypes.Value] s Text  ()
marketEndpoints market =  void selections
  where
    selections  =      sellEp market
              `select` buyEp market
              `select` listEp market
              `select` withdrawEp market
              `select` startAuctionEp market
              `select` bidEp market
              `select` claimEp market
              `select` withdrawEp market


sellEp :: HasEndpoint "sell" [SellParams] s =>Market -> Promise [AesonTypes.Value] s Text AesonTypes.Value
sellEp market =do
   (endpoint @"sell") $ \sps -> do
    ownPkh <-ownPubKey <&> pubKeyHash
    let sales=map (\x->(sellParamToDirectSale ownPkh x ,valueInfosToValue ( spItems x))) sps
    submitDirectSales market sales >>= confirmAndTell


buyEp :: (HasEndpoint "buy" PurchaseParam s) => Market -> Promise [AesonTypes.Value] s Text AesonTypes.Value
buyEp market= do
   (endpoint @"buy") $ \pp -> do
    findMarketUtxos market (ppItems  pp) >>= buyDirectSaleUtxos market >>= confirmAndTell

withdrawEp :: (HasEndpoint "withdraw" [TxOutRef] s) => Market -> Promise [AesonTypes.Value] s Text AesonTypes.Value
withdrawEp market=
  (endpoint @"withdraw")  $ \utxos -> do
    withdrawUtxos market utxos  >>= confirmAndTell

listEp :: (HasEndpoint "list" ListMarketRequest s)=> Market -> Promise [AesonTypes.Value] s Text AesonTypes.Value
listEp market = (endpoint @"list") handler

  where 
    handler (ListMarketRequest lType lMaybePkh lOwnPkh)=do
      ownPkh <-ownPubKey <&> pubKeyHash
      responses<-case lType of
                  MtDirectSale -> (case  lOwnPkh of
                                  Just True   ->  directSalesOfPkh market ownPkh
                                  _           -> (case lMaybePkh of 
                                                Just pkh -> directSalesOfPkh market $ PubKeyHash pkh
                                                _        -> directSalesInMarket market)

                                  )<&> map (directSaleToResponse  market) <&> toJSON
                  MtAuction   ->  (case  lOwnPkh of
                                    Just True ->  auctionsOfPkh market ownPkh
                                    _         ->(case lMaybePkh of 
                                                Just pkh -> auctionsOfPkh market $ PubKeyHash pkh
                                                _        -> auctionsInMarket market)
                                  )<&> map (auctionToResponse market)<&> toJSON 
      tell [responses]
      pure responses

startAuctionEp::  HasEndpoint "startAuction" [AuctionParam] s => Market -> Promise [AesonTypes.Value] s Text AesonTypes.Value
startAuctionEp market=
  endpoint @"startAuction" \aps -> do
    ownPkh <-ownPubKey <&> pubKeyHash
    let as= map (aParamToAuction ownPkh) aps
    submitAuction market as >>= confirmAndTell

bidEp::  HasEndpoint "bid" BidParam  s => Market -> Promise [AesonTypes.Value] s Text AesonTypes.Value
bidEp market=do
   endpoint @"bid" \(BidParam ref bid) -> do
    resolved <-parseMarketUtxo  market ref
    bidAuctionUtxo market resolved (valueInfosToValue bid)  >>= confirmAndTell

claimEp::  HasEndpoint "claim" ClaimParam  s => Market -> Promise [AesonTypes.Value] s Text AesonTypes.Value
claimEp market=do
   endpoint @"claim"  \(ClaimParam refs acceptError) -> do
    parsed <-(if acceptError then parseMarketUtxosNoError else parseMarketUtxos) market refs
    case parsed of 
      [] -> throwNoUtxo
      _ -> claimAuctionUtxos market parsed >>= confirmAndTell

confirmAndTell ::  AsContractError e =>  Tx -> Contract [AesonTypes.Value] s e AesonTypes.Value
confirmAndTell  tx = do
  let id=txId tx
  -- awaitTxConfirmed id -- TODO not sure if this is necessary.
  doReturn id

doReturn :: ToJSON a => a -> Contract [AesonTypes.Value] s e AesonTypes.Value
doReturn x =do
  tell [toJSON x]
  pure $ toJSON x