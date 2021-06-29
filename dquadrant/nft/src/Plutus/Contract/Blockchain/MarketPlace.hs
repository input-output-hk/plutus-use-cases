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
--     MarketScriptType,
--     mkMarket
-- )
where

import GHC.Generics (Generic)
import qualified Prelude (Show, Eq)
import PlutusTx.Prelude
import  PlutusTx
import Ledger
    ( getContinuingOutputs,
      txSignedBy,
      valuePaidTo,
      ScriptContext(ScriptContext, scriptContextTxInfo),
      TxInfo(txInfoValidRange),
      TxOut(txOutValue),
      Value,
      POSIXTimeRange,
      PubKeyHash,
      CurrencySymbol,
      TokenName,
      scriptAddress,
      contains,
      mkValidatorScript,
      Address,
      Validator,
      AssetClass, TxInInfo, toValidatorHash )
import Ledger.Value
import qualified Ledger.Typed.Scripts as Scripts
import Data.Aeson (FromJSON, ToJSON)
import Plutus.Contract.Data.Payment
import Plutus.Contract.Blockchain.Utils
import Ledger.Ada (adaSymbol,adaToken)
import Playground.Contract
import qualified PlutusTx.AssocMap as AssocMap
import Ledger.Contexts
    ( ScriptContext(ScriptContext, scriptContextTxInfo),
      getContinuingOutputs,
      ownHash,
      txSignedBy,
      valuePaidTo,
      TxInInfo(TxInInfo),
      TxInfo(TxInfo, txInfoInputs, txInfoValidRange),
      TxOut(TxOut, txOutValue) )


---------------------------------------------------------------------------------------------
----- Foreign functions (these use be in some other file but PLC plugin didn't agree)
---------------------------------------------------------------------------------------------

-- moving this function to Data/Payment.hs will give following error
--
--GHC Core to PLC plugin: E043:Error: Reference to a name which is not a local, a builtin, or an external INLINABLE function: 
-- Variable Plutus.Contract.Data.Payment.$s$fFoldable[]_$cfoldMap
--            OtherCon []
--Context: Compiling expr: Plutus.Contract.Data.Payment.$s$fFoldable[]_$cfoldMap

{-# INLINABLE validatePayment#-}
validatePayment :: (PubKeyHash ->  Value -> Bool )-> Payment ->Bool
validatePayment f p=
 all  (\pkh -> f pkh (paymentValue p pkh)) (paymentPkhs p)



-- moving this function to Blockchain/Utils.hs will give following error
--
--GHC Core to PLC plugin: E043:Error: Reference to a name which is not a local, a builtin,
--  or an external INLINABLE function: Variable 
--  Plutus.Contract.Blockchain.Utils.$s$fFoldable[]_$cfoldMap
--           No unfolding

{-# INLINABLE allowSingleScript #-}
allowSingleScript:: ScriptContext  -> Bool
allowSingleScript ctx@ScriptContext{scriptContextTxInfo=TxInfo{txInfoInputs}} =
     all checkScript txInfoInputs
  where
    checkScript (TxInInfo _ (TxOut address _ _))=
      case toValidatorHash   address of
        Just  vhash ->  traceIfFalse  "Reeming other Script utxos is Not allowed" (thisScriptHash == vhash)
        _           ->  True
    thisScriptHash= ownHash ctx

----------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

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

data MarketRedeemer =  ClaimBid| Bid | Buy | Withdraw | Collect
    deriving (Generic,FromJSON,ToJSON,Show,Prelude.Eq)

data SellType = Primary | Secondary  deriving (Show, Prelude.Eq,Generic,ToJSON,FromJSON,ToSchema)

data DirectSale=DirectSale{
    dsSeller:: !PubKeyHash,
    -- flattened singleton value
    dsCost ::  !Price,
    dsType::  !SellType
} deriving(Show,Generic,ToJSON,FromJSON)

{-# INLINABLE  dsSellerShare #-}
dsSellerShare :: Market -> DirectSale -> Integer
dsSellerShare Market{mPrimarySaleFee,mSecondarySaleFee} DirectSale{dsCost=Price (c,t,v),dsType}
    = ((marketHundredPercent-fee) * v)`divide` marketHundredPercent
    where
        fee= case dsType of
            Primary   ->  mPrimarySaleFee
            Secondary -> mSecondarySaleFee

{-# INLINABLE  dsSellerShareValue #-}
dsSellerShareValue :: Market -> DirectSale -> Value
dsSellerShareValue market ds@DirectSale{dsCost=Price(c,t,v)}=singleton c t $ dsSellerShare market ds

{-# INLINABLE dsMarketShare #-}
dsMarketShare :: Market -> DirectSale -> Integer
dsMarketShare market ds@DirectSale{dsCost=Price (_,_,v)}=
    v - dsSellerShare market ds

dsMarketShareValue :: Market -> DirectSale -> Value
{-# INLINABLE dsMarketShareValue #-}
dsMarketShareValue market ds@DirectSale{dsCost=Price(c,t,_)}=singleton c t $ dsMarketShare market ds

-- Previou's owner's winning after a auction is complete
{-# INLINABLE aSellerShareValue #-}
aSellerShareValue :: Market -> Auction -> Value-> Value
aSellerShareValue m@Market{mAuctionFee} a@Auction{aAssetClass,aValue} fValue =
  fValue - aValue-aMarketShareValue m a fValue


-- Operator's share for auction, 
-- if the split is fractional, market receives the extra. 
-- For example if market  fee is 3.22, operator will receive 4 Ada.
{-# INLINABLE aMarketShareValue #-}
aMarketShareValue :: Market -> Auction -> Value-> Value
aMarketShareValue Market{mAuctionFee} Auction{aAssetClass} fValue = assetClassValue aAssetClass v
    where
      v=finalAssetValue - (((marketHundredPercent - mAuctionFee) * finalAssetValue) `divide` marketHundredPercent )
      finalAssetValue= assetClassValueOf fValue aAssetClass


data Auction = Auction{
    aOwner  :: !PubKeyHash,
    aBidder:: !PubKeyHash,
    aAssetClass:: !AssetClass,
    aMinBid :: !Integer,
    aMinIncrement :: !Integer,
    aDuration:: !POSIXTimeRange,
    aValue:: Value
} deriving (Generic, Show,ToJSON,FromJSON,Prelude.Eq)
PlutusTx.unstableMakeIsData ''Auction


{-# INLINABLE auctionAssetValue #-}
auctionAssetValue :: Auction -> Integer -> Value
auctionAssetValue Auction{aAssetClass=AssetClass (c, t)} = singleton c t

{-# INLINABLE auctionAssetValueOf #-}
auctionAssetValueOf :: Auction -> Value -> Integer
auctionAssetValueOf Auction{aAssetClass} value = assetClassValueOf value aAssetClass


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

    -- without this check, auction creator might say that 
    -- they are placing asset on auction datum without locking them.
    validInputDatum = ownInputValue ctx `geq` aValue auction
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
            allowSingleScript ctx
        &&  traceIfFalse "Malicious input datum" validInputDatum
        &&  traceIfFalse "Only one bid per transaction" hasSingleUtxo
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


{-# INLINABLE  validateWithdraw #-}
validateWithdraw :: Market->Data -> ScriptContext -> Bool
validateWithdraw market datum ctx=
          isDirectSale
      ||  isAuction
      || traceError  "Only Operator can withdraw utxos with invalid datum" (txSignedBy info $ mOperator market)
  where
      isAuction = case fromData datum of
          (Just auction)      ->  traceIfFalse "Missing owner signature" (txSignedBy info (aOwner auction)) &&
                                  traceIfFalse "Cannot withdraw auction with bids" (aBidder auction==aOwner auction) &&
                                  traceIfFalse "Auction is Still active"  (auctionNotActive auction)
          _ -> False
      isDirectSale= case fromData  datum of
          (Just directSale)   -> txSignedBy info $ dsSeller directSale
          _                   -> False
      info=scriptContextTxInfo ctx
      auctionNotActive auction = not $ aDuration auction `contains` txInfoValidRange info


{-# INLINABLE validateClaimAuction  #-}
validateClaimAuction :: Market  -> ScriptContext -> Bool
validateClaimAuction  market@Market{mAuctionFee,mOperator} ctx@ScriptContext{scriptContextTxInfo=info} =
          allowSingleScript ctx
      &&  traceIfTrue  "Auction not Completed" allAuctionsNotExpired
      &&  traceIfFalse "Market fee not paid" isOperatorPaid
      &&  traceIfFalse "Bidder not paid"     areWinnersPaid
      && traceIfFalse  "Is Seller Paid"      areSellersPaid
      where
        -- auction validity
        allAuctionsNotExpired = all isAuctionPeriod auctionsWithTxOut
        isAuctionPeriod (txOut,auction) = not $ aDuration auction `contains` txInfoValidRange info

        -- Check that each of the parties are paid
        areWinnersPaid  = validatePayment (\pkh v->valuePaidTo info pkh `geq` v)  totalWinnerPayment
        areSellersPaid  = validatePayment (\pkh v -> valuePaidTo info pkh  `geq` v)  totalSellerPayment
        isOperatorPaid  = valuePaidTo info mOperator `geq`  totalOperatorFee

        -- Total payments arising from the utxos
        totalSellerPayment= foldMap  sellerPayment auctionsWithTxOut
        totalWinnerPayment= foldMap  aWinnerPayment auctionsWithTxOut
        totalOperatorFee  = foldMap  operatorFee auctionsWithTxOut

        -- payment share for each party in a auction txOut
        sellerPayment   (txOut,auction) = payment  (aOwner auction) $ aSellerShareValue market auction $ txOutValue txOut
        operatorFee     (txOut,auction) = aMarketShareValue market auction $ txOutValue txOut
        aWinnerPayment  (txOut,auction) = payment (aBidder auction) $ aValue auction

        auctionsWithTxOut:: [(TxOut,Auction)]
        auctionsWithTxOut=ownInputsWithDatum ctx

{-# INLINABLE validateBuy #-}
validateBuy:: Market -> ScriptContext ->Bool
validateBuy market@Market{mOperator,mPrimarySaleFee,mSecondarySaleFee} ctx=
       allowSingleScript ctx
    && traceIfFalse "Insufficient payment" areSellersPaid
    && traceIfFalse "Insufficient fees" isMarketFeePaid
    where
        info=scriptContextTxInfo ctx

        isMarketFeePaid = valuePaidTo info mOperator `geq` totalMarketFee
        areSellersPaid  = validatePayment (\pkh v-> valuePaidTo info pkh `geq` v) totalSellerPayment

        totalSellerPayment  = foldMap  sellerSharePayment salesWithTxOut
        totalMarketFee      = foldMap  marketFeeValue salesWithTxOut

        sellerSharePayment (txOut,dsale) = payment (dsSeller dsale)  $  dsSellerShareValue market dsale
        marketFeeValue    (txOut,dsale)  = dsMarketShareValue market dsale

        salesWithTxOut :: [(TxOut,DirectSale)]
        salesWithTxOut= ownInputsWithDatum ctx

{-# INLINABLE mkMarket #-}
mkMarket :: Market ->  Data -> MarketRedeemer -> ScriptContext  -> Bool
mkMarket market d action ctx = 
    case  action of
        Buy       -> validateBuy market ctx
        Withdraw -> validateWithdraw market d ctx
        Bid       -> case fromData d of
                    Just auction -> validateBid market auction ctx
                    _            -> traceError "Invalid Auction datum"
        ClaimBid  -> validateClaimAuction market ctx


data MarketScriptType
instance Scripts.ValidatorTypes MarketScriptType where
    type instance RedeemerType MarketScriptType = MarketRedeemer
    type instance DatumType MarketScriptType = PubKeyHash


marketValidator :: Market -> Validator
marketValidator market = Ledger.mkValidatorScript $
    $$(PlutusTx.compile [|| validatorParam ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode market
    where validatorParam m = Scripts.wrapValidator (mkMarket m)


-- marketValidator :: Market -> Validator
-- marketValidator market= mkValidatorScript $$(PlutusTx.compile [||a ||])
--     where
--         a _ _ _=()

marketAddress :: Market -> Ledger.Address
marketAddress = scriptAddress . marketValidator

PlutusTx.unstableMakeIsData ''Market
PlutusTx.unstableMakeIsData ''MarketRedeemer
PlutusTx.unstableMakeIsData ''SellType
PlutusTx.unstableMakeIsData ''Price
PlutusTx.unstableMakeIsData ''DirectSale

PlutusTx.makeLift ''Market
PlutusTx.makeLift ''MarketRedeemer
PlutusTx.makeLift ''SellType
PlutusTx.makeLift ''Price
PlutusTx.makeLift ''DirectSale
PlutusTx.makeLift ''Auction