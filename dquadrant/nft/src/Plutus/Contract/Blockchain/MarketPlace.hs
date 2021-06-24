{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores#-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Plutus.Contract.Blockchain.MarketPlace
--     Price(..),
--     valueOfPrice,
--     Market(..),
--     DirectSale(..),
--     SellType(..),
--     Auction(..),
--     MarketRedeemer(..),
--     marketValidator,
--     marketAddress,
--     -- TODO remove the exports below
--     MarketType,
--     mkMarket
-- )
where

import GHC.Generics (Generic)
import qualified Prelude (Show, Eq)
-- import Prelude(div)
import PlutusTx.Prelude
import  PlutusTx
import Ledger hiding(singleton,txOutDatum)
import Ledger.Value
import qualified Ledger.Typed.Scripts as Scripts
import Data.Aeson (FromJSON, ToJSON)
import Plutus.Contract.Data.Payment
import Plutus.Contract.Blockchain.Utils
import Ledger.Ada (adaSymbol,adaToken)
import Playground.Contract
import qualified PlutusTx.AssocMap as AssocMap

{-# INLINABLE marketHundredPercent #-}
marketHundredPercent :: Integer
marketHundredPercent=100_000_000

newtype Price = Price (CurrencySymbol ,TokenName ,Integer) deriving(Show,Generic,ToJSON,FromJSON)

{-# INLINABLE valueOfPrice#-}
valueOfPrice :: Price ->  Value
valueOfPrice (Price (c,t,v)) = singleton c t v


data Market = Market
    {   mOperator           :: !PubKeyHash
    ,   mPrimarySaleFee     :: !Integer
    ,   mSecondarySaleFee   :: !Integer
    ,   mAuctionFee         :: !Integer
    } deriving (Show,Generic, FromJSON, ToJSON)

data MarketRedeemer =  ClaimBid| Bid | Buy | TakeBack | Collect
    deriving (Generic,FromJSON,ToJSON,Show,Prelude.Eq)

data MarketData =IsDs DirectSale  | IsAuction Auction | Unspecified deriving (Show,Generic,ToJSON,FromJSON)

data SellType = Primary | Secondary  deriving (Show, Prelude.Eq,Generic,ToJSON,FromJSON,ToSchema)

data DirectSale=DirectSale{
    dsSeller:: !PubKeyHash,
    -- flattened value
    dsCost ::  !Price,
    dsType::  !SellType
} deriving(Show,Generic,ToJSON,FromJSON)

{-# INLINABLE  dsUserShare #-}

dsUserShare :: Market -> DirectSale -> Integer
dsUserShare Market{mPrimarySaleFee,mSecondarySaleFee} DirectSale{dsCost=Price (c,t,v),dsType}
    = ((marketHundredPercent-fee) * v)`divide` marketHundredPercent
    where
        fee= case dsType of
            Primary   ->  mPrimarySaleFee
            Secondary -> mSecondarySaleFee

{-# INLINABLE  dsUserShareValue #-}
dsUserShareValue :: Market -> DirectSale -> Value
dsUserShareValue market ds@DirectSale{dsCost=Price(c,t,v)}=singleton c t $ dsUserShare market ds

{-# INLINABLE dsMarketShare #-}
dsMarketShare :: Market -> DirectSale -> Integer
dsMarketShare market ds@DirectSale{dsCost=Price (_,_,v)}=
    v- dsUserShare market ds

dsMarketShareValue :: Market -> DirectSale -> Value
{-# INLINABLE dsMarketShareValue #-}
dsMarketShareValue market ds@DirectSale{dsCost=Price(c,t,_)}=singleton c t $ dsMarketShare market ds

aUserShareValue :: Market -> Auction -> Value-> Value
aUserShareValue Market{mAuctionFee} Auction{aAssetClass} fValue = fValue <> assetClassValue aAssetClass (-v)
    where 
      v=(marketHundredPercent - mAuctionFee) * finalAssetValue `divide` marketHundredPercent
      finalAssetValue= assetClassValueOf fValue aAssetClass

aMarketShareValue :: Market -> Auction -> Value-> Value
aMarketShareValue Market{mAuctionFee} Auction{aAssetClass} fValue = assetClassValue aAssetClass v
    where 
      v=finalAssetValue - (marketHundredPercent - mAuctionFee) * finalAssetValue `divide` marketHundredPercent
      finalAssetValue= assetClassValueOf fValue aAssetClass


data Auction = Auction{
    aOwner  :: !PubKeyHash,
    aBidder:: !PubKeyHash,
    aAssetClass:: !AssetClass,
    aMinBid :: !Integer,
    aMinIncrement :: !Integer,
    aDuration:: !POSIXTimeRange,
    aValue:: Value
} deriving (Generic, Show,ToJSON,FromJSON)
PlutusTx.unstableMakeIsData ''Auction


{-# INLINABLE auctionAssetValue #-}
auctionAssetValue :: Auction -> Integer -> Value
auctionAssetValue Auction{aAssetClass=AssetClass (c, t)} = singleton c t

{-# INLINABLE auctionAssetValueOf #-}
auctionAssetValueOf :: Auction -> Value -> Integer
auctionAssetValueOf Auction{aAssetClass} value = assetClassValueOf value aAssetClass



newtype Payment = Payment ( AssocMap.Map PubKeyHash Value ) deriving(Generic,ToJSON,FromJSON,Show)

instance Semigroup Payment where
    {-# INLINABLE (<>) #-}
    (<>) (Payment a) (Payment b) = Payment (a <> b)

instance Monoid Payment where
  {-# INLINABLE mempty   #-}
  mempty = Payment AssocMap.empty 

{-# INLINABLE payment  #-}
payment :: PubKeyHash -> Value -> Payment
payment pkHash value=Payment  (AssocMap.singleton pkHash value)

{-# INLINABLE paymentValue #-}
paymentValue :: Payment -> PubKeyHash -> Value
paymentValue (Payment p) pkh=case AssocMap.lookup pkh p of
    Just v ->  v
    _      ->Value AssocMap.empty

{-# INLINABLE paymentPkhs #-}
paymentPkhs :: Payment -> [PubKeyHash]
paymentPkhs (Payment x) =  AssocMap.keys x


validatePayment :: (PubKeyHash ->  Value -> Bool )-> Payment ->Bool
validatePayment f p=
  all (\pkh -> f pkh (paymentValue p pkh)) (paymentPkhs p)

makeLift ''Payment
PlutusTx.unstableMakeIsData ''Payment




{-# INLINABLE  validateBid #-}
validateBid :: Market -> Auction -> ScriptContext -> Bool
validateBid market auction ctx@ScriptContext  {scriptContextTxInfo=info}=
  let
    hasSingleUtxo=    length (ownInputs ctx) == 1
    duringTheValidity  =   aDuration auction `contains` txInfoValidRange info
    validOutputDatum    =  case txOutDatum ctx newTxOut of
        Just nAuction@Auction{} ->  aMinIncrement auction == aMinIncrement nAuction &&
                                      aAssetClass auction == aAssetClass nAuction &&
                                      aDuration auction == aDuration nAuction &&
                                      aOwner auction== aOwner auction
        _                        ->  traceError "Malformed Output Datum"
    minNewBid =
        ownInputValue ctx <>
        auctionAssetValue auction (
            if  lastAuctionAssetValue == 0
            then  aMinBid auction
            else  aMinIncrement auction)

    isExBidderPaid=
        if lastAuctionAssetValue == 0
        then True
        else assetClassValueOf  (valuePaidTo info (aBidder auction))  (aAssetClass auction) >= lastAuctionAssetValue

    lastAuctionAssetValue= assetClassValueOf  (ownInputValue ctx ) (aAssetClass auction)

    isMarketScriptPayed nAuction= ownOutputValue ctx `geq` minNewBid

    doValidate newAuction=
            traceIfFalse "Only one bid per transaction" hasSingleUtxo
        &&  traceIfFalse "Insufficient payment to market contract" (isMarketScriptPayed newAuction)
        &&  traceIfFalse "Insufficient payment to previous bidder" isExBidderPaid
        &&  traceIfFalse "Not during the auction period" duringTheValidity
        &&  traceIfFalse "Unacceptible modification to output datum" validOutputDatum
    newTxOut=(case getContinuingOutputs ctx of
        [txOut] -> txOut
        _       -> traceError "MultipleOutputs"
      )
  in
    case txOutDatum  ctx newTxOut of
          Just nAuction@Auction{} -> doValidate nAuction
          _       -> traceError "Multiple outputs"


{-# INLINABLE  validateTakeback #-}
validateTakeback :: Data -> ScriptContext -> Bool
validateTakeback datum ctx= isAuction || isDirectSale || traceError "Invalid Datum in tx when withdrawing"
  where
      isAuction = case fromData datum of
          (Just auction)      -> txSignedBy info (aBidder auction) &&
                                    auctionNotActive auction
          _ -> False
      isDirectSale= case fromData  datum of
          (Just directSale)   -> txSignedBy info $ dsSeller directSale
          _                   -> False
      info=scriptContextTxInfo ctx
      auctionNotActive auction = not $ aDuration auction `contains` txInfoValidRange info


{-# INLINABLE validateClaimAuction  #-}
validateClaimAuction :: Market -> Data -> ScriptContext -> Bool
validateClaimAuction  market@Market{mAuctionFee,mOperator} datum ctx@ScriptContext{scriptContextTxInfo=info} =
  case fromData datum of
    Just auction ->
          traceIfTrue  "Auction not Completed" (not  isAuctionPeriod)
      &&  traceIfFalse "Market fee not paid" isOperatorPaid
      &&  traceIfFalse "Bidder not paid"     areWinnersPaid
      && traceIfFalse  "Is Seller Paid"      areSellersPaid
      where
        isAuctionPeriod = not $ aDuration auction `contains` txInfoValidRange info

        -- Check that each of the parties are paid
        areWinnersPaid  = validatePayment (\pkh v->valuePaidTo info pkh `geq` v)  totalWinnerPayment
        isOperatorPaid  = valuePaidTo info mOperator `geq`  totalOperatorFee
        areSellersPaid  = validatePayment (\pkh v -> valuePaidTo info pkh  `geq` v)  totalSellerPayment

        -- Total payments arising from the utxos
        totalSellerPayment= mconcat $ mapInputsWithDatum sellerPayment
        totalWinnerPayment= mconcat $ mapInputsWithDatum aWinnerPayment
        totalOperatorFee  = mconcat $ mapInputsWithDatum operatorFee

        -- payment share for each party in a auction txOut
        sellerPayment   txOut auction = payment  (aOwner auction) $ aUserShareValue market auction $ txOutValue txOut
        operatorFee     txOut auction = aMarketShareValue market auction $ txOutValue txOut
        aWinnerPayment  txOut auction = payment (aBidder auction) $ aUserShareValue market auction $ aValue auction
      


        marketExpectation txOut@TxOut{txOutValue} =do
          auction <- txOutDatum  ctx txOut
          let aClass=aAssetClass auction
          pure $ assetClassValue aClass (marketShare (assetClassValueOf txOutValue aClass))

        marketShare x = x - userShare x
        -- userShare x =x*(1000000-x)  `div` 1000000
        mapInputsWithDatum :: IsData b => (TxOut->b->c) -> [c]
        mapInputsWithDatum f= mapMaybe (\txo->txOutDatum ctx txo >>=(\d->Just $ f txo d)) $ ownInputs ctx

        userShare x=x
    _       ->
        traceError "Invalid input datum"

{-# INLINABLE validateBuy #-}
validateBuy:: Market -> ScriptContext ->Bool
validateBuy market@Market{mOperator,mPrimarySaleFee,mSecondarySaleFee} ctx=
        -- traceIfFalse "Insufficient payment" allSellersPaid
    traceIfFalse "Insufficient fees" isMarketPayed
    where
        info=scriptContextTxInfo ctx

        marketInputs::[TxOut]
        marketInputs = ownInputs ctx

        allSellersPaid::Bool
        allSellersPaid = foldl (\truth pkh ->truth && isSellerPaid pkh) True  (paymentPkhs sellerExpectations)

        isSellerPaid:: PubKeyHash ->Bool
        isSellerPaid pkh=
            valuePaidTo info pkh
                `geq`
                paymentValue sellerExpectations pkh

        isMarketPayed::  Bool
        isMarketPayed= valueOf  (valuePaidTo info mOperator) adaSymbol adaToken
                             >=
                        (sum  $ map (\a->marketExpectation a) marketInputs)

        sellerExpectations :: Payment
        sellerExpectations =mconcat $ map (\a->paymentInfo a) marketInputs

        paymentInfo :: TxOut  ->  Payment
        paymentInfo  txOut = case txOutDatum ctx txOut  of
            Just ds@DirectSale{dsCost=Price(c,t,v)} ->
                 payment (dsSeller ds) $ singleton c t (dsUserShare market ds)
            _                 -> traceError "No Cost info"

        marketExpectation :: TxOut  -> Integer
        marketExpectation txOut = case txOutDatum ctx txOut of
            Just ds ->  dsMarketShare market ds
            _                 -> traceError "No Cost info"


{-# INLINABLE mkMarket #-}
mkMarket :: Market ->  Data -> MarketRedeemer -> ScriptContext  -> Bool
mkMarket market d action ctx =
    case  action of
        Buy       -> validateBuy market ctx
        TakeBack -> validateTakeback d ctx
        Bid       -> case fromData d of
                    Just auction -> validateBid market auction ctx
                    _            -> traceError "Invalid Auction datum"
        ClaimBid  -> validateClaimAuction market d ctx


data MarketType
instance Scripts.ValidatorTypes MarketType where
    type instance RedeemerType MarketType = MarketRedeemer
    type instance DatumType MarketType = PubKeyHash


-- marketValidator :: Market -> Validator
-- marketValidator market = Ledger.mkValidatorScript $
--     $$(PlutusTx.compile [|| validatorParam ||])
--         `PlutusTx.applyCode`
--             PlutusTx.liftCode market
--     where validatorParam m = Scripts.wrapValidator (mkMarket m)


marketValidator :: Market -> Validator
marketValidator market= mkValidatorScript $$(PlutusTx.compile [||a ||])
    where
        a _ _ _=()

marketAddress :: Market -> Ledger.Address
marketAddress = scriptAddress . marketValidator


PlutusTx.unstableMakeIsData ''Market
PlutusTx.unstableMakeIsData ''MarketRedeemer
PlutusTx.unstableMakeIsData ''SellType
PlutusTx.unstableMakeIsData ''Price
PlutusTx.unstableMakeIsData ''DirectSale
PlutusTx.unstableMakeIsData ''MarketData

PlutusTx.makeLift ''MarketData
PlutusTx.makeLift ''Market
PlutusTx.makeLift ''MarketRedeemer
PlutusTx.makeLift ''SellType
PlutusTx.makeLift ''Price
PlutusTx.makeLift ''DirectSale
PlutusTx.makeLift ''Auction
