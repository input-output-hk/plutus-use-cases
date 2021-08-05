{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Plutus.Contract.Wallet.MarketPlace(
    submitDirectSales,
    withdrawUtxos,
    findMarketUtxos,
    buyDirectSaleUtxos,
    directSalesOfPkh,
    directSalesInMarket,
    auctionsInMarket,
    auctionsOfPkh,
    submitAuction,
    bidAuctionUtxo,
    parseMarketUtxosNoError,
    parseMarketUtxos,
    parseMarketUtxo,
    claimAuctionUtxos,
    marketValidator,
    Market (..),
    marketAddress,
    DirectSale (..),
    Auction(..),
    SellType(..),
    Price,
    valueOfPrice,
)

where
import Plutus.Contract.Blockchain.MarketPlace
import Plutus.Contract.Blockchain.Utils
import Plutus.Contract
import Ledger.Value
import Ledger  hiding(TxOutRefNotFound, unspentOutputs,txOutDatum)
import qualified Data.Map as Map
import PlutusTx.Prelude hiding ((<>))
import qualified Data.Aeson.Types as JsonTypes
import Data.Aeson (FromJSON,ToJSON, toJSON)
import  Ledger.Scripts
import Data.Text (Text, singleton)
import Prelude (String,Show, show, (<>), Monoid (mconcat), Foldable (elem, length), concat)
import Ledger.AddressMap (UtxoMap)
import GHC.Generics (Generic)
import PlutusTx (toData, IsData)
import  Control.Monad.Error.Lens (throwing, throwing_)
import Control.Monad
import Plutus.Contract.Wallet.Utils

-- TODO remove imports below
import qualified PlutusTx
import PlutusTx.Data
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Ada (lovelaceValueOf)
import Plutus.Contract.Constraints
import qualified Data.Set as Set
import Data.Functor
import Ledger.TimeSlot (posixTimeRangeToSlotRange, slotToPOSIXTime)
import Control.Lens (review)


-- Create direct sale. in the tuple (DirectSale,Value), DirectSale contains 
-- data required to validate buy  action. Value is the amount to be put on directsale.
submitDirectSales:: (AsContractError  e) => Market ->[(DirectSale ,Value)] -> Contract w s e Tx
submitDirectSales market sps=do submitTx $ Prelude.mconcat $ map toConstraint sps
  where
    toConstraint (ds,v)=mustPayToOtherScript valHash (Datum $ toData  ds) v
    valHash=validatorHash $ marketValidator market


buyDirectSaleUtxos :: (AsContractError e) => Market -> [ParsedUtxo DirectSale]  -> Contract w s e Tx
buyDirectSaleUtxos m fUtxos= submitTxConstraintsWith @MarketScriptType lookups tx
    where
        tx=foldMap  toConstraint fUtxos
        lookups=otherScript (marketValidator m )Prelude.<> unspentOutputs consumedOutputs

        consumedOutputs=Map.fromList $ map (\(a,b,c) ->(a,b)) fUtxos

        toConstraint (utxoRef, _, ds) = 
            mustSpendScriptOutput utxoRef (Redeemer ( toData Buy))
          <> dsSellerPayments ds
          <> mustPayToPubKey (mOperator m) (assetClassValue (dsAsset ds) $  dsMarketShare m ds)

        dsSellerPayments (DirectSale parties ac _)=foldMap (\(pkh,v)-> mustPayToPubKey pkh  (assetClassValue ac v)) parties


submitAuction :: AsContractError e => Market -> [Auction] -> Contract w s e Tx
submitAuction market  as = submitTx $ Prelude.mconcat $ map constraint as
  where
    constraint auction = mustPayToOtherScript (validatorHash $ marketValidator market) (Datum $ toData auction) $ aValue auction

bidAuctionUtxo :: AsContractError e => Market -> ParsedUtxo Auction ->Value -> Contract w s e Tx
bidAuctionUtxo market (ref,tx@TxOutTx{txOutTxOut=utxo},ac) bidAmount = do
  ownPkh <- ownPubKey <&> pubKeyHash
  slot <-currentSlot
  let newAuction  = Auction{
              aOwner        = aOwner ac,
              aParties      = aParties ac,
              aBidder       = ownPkh,
              aAssetClass   = aAssetClass ac,
              aMinBid       = aMinBid  ac,
              aMinIncrement = aMinIncrement ac,
              aDuration     = aDuration ac,
              aValue        = if isFirstBid then  txOutValue utxo else aValue ac
        }

      constraintsExceptLastBidder=
        mustSpendScriptOutput ref (Redeemer ( toData Bid))
        <> (mustPayToOtherScript (validatorHash $ marketValidator market) (Datum $ toData newAuction) $ scriptShareValue)
        <> mustValidateIn (posixTimeRangeToSlotRange (aDuration ac))

      constraints= if isFirstBid  then constraintsExceptLastBidder else
                        constraintsExceptLastBidder<> mustPayToPubKey  (aBidder  ac) lastBidderShareValue

  submitTxConstraintsWith @MarketScriptType lookups constraints
  where
      lookups=otherScript (marketValidator market ) Prelude.<>  unspentOutputs (Map.singleton ref tx)
      lastBidderShare       = auctionAssetValueOf ac  (txOutValue utxo)
      lastBidderShareValue  = auctionAssetValue ac lastBidderShare
      scriptShareValue      = auctionAssetValue ac (-lastBidderShare) <> lastValue <> bidAmount
      lastValue=txOutValue utxo
      isFirstBid= lastBidderShare == 0 && (aBidder  ac == aOwner ac)


claimAuctionUtxos ::AsContractError e => Market -> [ParsedUtxo Auction] -> Contract w s e Tx
claimAuctionUtxos market refs@[(_,_,a)] =  logInfo (show constraint)>> logInfo  a>> submitTxConstraintsWith @MarketScriptType lookups constraint
  where
  lookups=otherScript (marketValidator market ) Prelude.<>  unspentOutputs uTxoLookup

  constraint=foldMap utxoToConstraint refs

  uTxoLookup=Map.fromList $ map (\(a,b,c) ->(a,b)) refs

  utxoToConstraint  (txOutRef,TxOutTx _ (TxOut _ value _), auction)=
    mustSpendScriptOutput txOutRef (Redeemer $ toData ClaimBid )
    <> mustPayToPubKey (aBidder auction) (aValue auction)
    <> mustPayToPubKey (aOwner auction) (aSellerShareValue  market auction  value )
    <> mustPayToPubKey  (mOperator market) (aMarketShareValue market auction value)
    <> mustValidateIn ( posixTimeRangeToSlotRange $ aClaimInterval auction)


withdrawUtxos :: (AsContractError e) => Market -> [TxOutRef]  -> Contract w s e Tx
withdrawUtxos market refs=do
  utxoMap<-utxoAt  $ marketAddress market
  let resolvedUtxos = mapMaybe (\utxo ->Map.lookup utxo utxoMap<&>(utxo,)) refs

  let lookups =(otherScript $ marketValidator market) <> unspentOutputs (Map.fromList resolvedUtxos)
  let constraints =Prelude.mconcat $ map (\(u,_)->mustSpendScriptOutput u redeemer) resolvedUtxos
  submitTxConstraintsWith @MarketScriptType lookups constraints
  where
    redeemer= Redeemer $ toData Withdraw

parseMarketUtxo::(IsData a,AsContractError e) => Market -> TxOutRef  -> Contract w s e (TxOutRef ,TxOutTx,a)
parseMarketUtxo market =resolveRefWithDataAt (marketAddress market)

parseMarketUtxos::(IsData a,AsContractError e) => Market -> [TxOutRef]  -> Contract w s e [ParsedUtxo a]
parseMarketUtxos market= resolveRefsWithDataAt (marketAddress market)

parseMarketUtxosNoError ::(IsData a,AsContractError e) => Market -> [TxOutRef]  -> Contract w s e [ParsedUtxo a]
parseMarketUtxosNoError market = resolveRefsWithDataAtWithError (marketAddress market)


directSalesInMarket ::  (AsContractError e) =>
             Market
             -> Contract w s e  [ParsedUtxo DirectSale]
directSalesInMarket market = utxosWithDataAt (marketAddress market)

auctionsInMarket :: (AsContractError e) =>
             Market
             -> Contract w s e  [ParsedUtxo Auction]
auctionsInMarket market= utxosWithDataAt (marketAddress market)

auctionsOfPkh ::(AsContractError e) =>
             Market  ->PubKeyHash-> Contract w s e [ParsedUtxo Auction]
auctionsOfPkh market pkh = auctionsInMarket market <&> filter (\(_,_,a)-> aOwner  a==pkh || aBidder a== pkh )

directSalesOfPkh ::(AsContractError e) =>
             Market  ->PubKeyHash-> Contract w s e [ParsedUtxo DirectSale]
directSalesOfPkh market pkh = directSalesInMarket market <&> filter (\(_,_,ds)->   pkh `Prelude.elem` (map fst $ dsParties ds))

findMarketUtxos:: (AsContractError e,IsData st) => Market -> [TxOutRef] -> Contract w s e [ParsedUtxo st]
findMarketUtxos market txouts =do
      let items = Set.fromList txouts
      filterUtxosWithDataAt (\x _ ->  Prelude.elem x items) $ marketAddress market