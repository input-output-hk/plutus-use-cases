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
{-# LANGUAGE FlexibleContexts  #-}
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
import PlutusTx.Prelude
import qualified Prelude
import  PlutusTx hiding( txOutDatum)
import Ledger hiding(txOutDatum)
import Ledger.Value hiding(lt)
import Ledger.Credential
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
import Ledger.Interval (UpperBound(UpperBound),LowerBound(LowerBound))
import Ledger.Time (POSIXTime)
import qualified Data.Bifunctor


---------------------------------------------------------------------------------------------
----- Foreign functions (these used be in some other file but PLC plugin didn't agree)
---------------------------------------------------------------------------------------------

-- Payment.hs

instance Semigroup Payment where
    {-# INLINABLE (<>) #-}
    (<>) (Payment a) (Payment b) = Payment (a <> b)

instance Monoid Payment where
  {-# INLINABLE mempty   #-}
  mempty = Payment AssocMap.empty


{-# INLINABLE payment  #-}
payment :: PubKeyHash -> Value -> Payment
payment pkHash value=Payment  (AssocMap.singleton pkHash value)

{-# INLINABLE assetClassPayment #-}
assetClassPayment :: AssetClass  -> [(PubKeyHash,Integer)] -> Payment
assetClassPayment ac values=Payment (AssocMap.fromList mappedList)
  where
    mappedList= map (Data.Bifunctor.second (assetClassValue ac)) values

{-# INLINABLE paymentValue #-}
paymentValue :: Payment -> PubKeyHash -> Value
paymentValue (Payment p) pkh=case AssocMap.lookup pkh p of
    Just v ->  v
    _      ->Value AssocMap.empty

{-# INLINABLE paymentPkhs #-}
paymentPkhs :: Payment -> [PubKeyHash]
paymentPkhs (Payment x) =  AssocMap.keys x

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


--Utils.hs
-- address of this validator
{-# INLINABLE ownAddress #-}
ownAddress :: ScriptContext -> Address
ownAddress ctx=scriptHashAddress (ownHash ctx)

-- all the utxos that are being redeemed from this contract in this transaction
{-# INLINABLE  ownInputs #-}
ownInputs:: ScriptContext -> [TxOut]
ownInputs ctx@ScriptContext{scriptContextTxInfo=TxInfo{txInfoInputs}}=
     filter (\x->txOutAddress x==ownAddress ctx) resolved
    where
    resolved=map (\x->txInInfoResolved x) txInfoInputs

-- get List of valid parsed datums  the script in this transaction
{-# INLINABLE ownInputDatums #-}
ownInputDatums :: FromData a => ScriptContext  -> [a]
ownInputDatums ctx= mapMaybe (txOutDatum ctx) $  ownInputs ctx

-- get List of the parsed datums  including the TxOut if datum is valid
{-# INLINABLE ownInputsWithDatum #-}
maybeOwnInputsWithDatum:: FromData a =>  ScriptContext ->[Maybe (TxOut,a)]
maybeOwnInputsWithDatum ctx=map (txOutWithDatum ctx)  ( ownInputs ctx)

ownInputsWithDatum:: FromData a=> ScriptContext  -> [(TxOut,a)]
ownInputsWithDatum ctx= map doValidate (ownInputs ctx)
  where
    doValidate:: FromData a =>  TxOut -> (TxOut,a)
    doValidate txOut = case txOutWithDatum ctx txOut of
      Just a -> a
      _      -> traceError "Datum format in Utxo is not of required type"

-- get input datum for the utxo that is currently being validated
{-# INLINABLE ownInputDatum #-}
ownInputDatum :: FromData a => ScriptContext -> Maybe a
ownInputDatum ctx = do
    txInfo <-findOwnInput ctx
    let txOut= txInInfoResolved txInfo
    txOutDatum ctx txOut

--  given an Utxo, resolve it's datum to our type
{-# INLINABLE txOutDatum #-}
txOutDatum::  FromData a =>  ScriptContext ->TxOut -> Maybe a
txOutDatum ctx txOut =do
            dHash<-txOutDatumHash txOut
            datum<-findDatum dHash (scriptContextTxInfo ctx)
            PlutusTx.fromBuiltinData $ getDatum datum

-- given txOut get resolve it to our type and return it with the txout
{-# INLINABLE txOutWithDatum #-}
txOutWithDatum::  FromData a =>  ScriptContext ->TxOut -> Maybe (TxOut,a)
txOutWithDatum ctx txOut =do
            d<-txOutDatum ctx txOut
            return (txOut,d)

--  value that is being redeemed from this contract in this utxo
{-# INLINABLE ownInputValue #-}
ownInputValue:: ScriptContext -> Value
ownInputValue ctx = case  findOwnInput ctx of
      Just TxInInfo{txInInfoResolved} ->  txOutValue txInInfoResolved

-- total value that will be locked by this contract in this transaction
{-# INLINABLE  ownOutputValue #-}
ownOutputValue :: ScriptContext -> Value
ownOutputValue ctx = valueLockedBy (scriptContextTxInfo ctx) (ownHash ctx)


{-# INLINABLE allowSingleScript #-}
allowSingleScript:: ScriptContext  -> Bool
allowSingleScript ctx@ScriptContext{scriptContextTxInfo=TxInfo{txInfoInputs}} =
    all checkScript txInfoInputs
  where
    checkScript (TxInInfo _ (TxOut address _ _))=
      case addressCredential  address of
        ScriptCredential vhash ->  traceIfFalse  "Reeming other Script utxo is Not allowed" (thisScriptHash == vhash)
        _ -> True
    thisScriptHash= ownHash ctx


allScriptInputsCount:: ScriptContext ->Integer
allScriptInputsCount ctx@(ScriptContext info purpose)=
    foldl (\c txOutTx-> c + countTxOut txOutTx) 0 (txInfoInputs  info)
  where
  countTxOut (TxInInfo _ (TxOut addr _ _)) = if isJust (toValidatorHash addr) then 1 else 0


----------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

{-# INLINABLE marketHundredPercent #-}
marketHundredPercent :: Percent
marketHundredPercent=100_000_000

percent:: Integer -> Percent
percent a = a * 1_000_000


newtype Price = Price (CurrencySymbol ,TokenName ,Integer) deriving(Show,Generic,ToJSON,FromJSON)
type Percent = Integer

{-# INLINABLE valueOfPrice#-}
valueOfPrice :: Price ->  Value
valueOfPrice (Price (c,t,v)) = Ledger.Value.singleton c t v


data Market = Market
    {   mOperator           :: !PubKeyHash
    ,   mPrimarySaleFee     :: !Integer
    ,   mSecondarySaleFee   :: !Integer
    ,   mAuctionFee         :: !Integer
    } deriving (Show,Generic, FromJSON, ToJSON)

PlutusTx.makeLift ''Market
PlutusTx.unstableMakeIsData ''Market


data MarketRedeemer =  ClaimBid| Bid | Buy | Withdraw
    deriving (Generic,FromJSON,ToJSON,Show,Prelude.Eq)

PlutusTx.unstableMakeIsData ''MarketRedeemer

data SellType = Primary | Secondary  deriving (Show,Generic,ToJSON,FromJSON,ToSchema,Prelude.Eq)
PlutusTx.unstableMakeIsData ''SellType

type PartyShare=(PubKeyHash,Percent)
PlutusTx.unstableMakeIsData ''Price

data DirectSale=DirectSale{
    dsSeller:: PubKeyHash, -- The mail seller
    dsParties::  [(PubKeyHash,Percent)], -- Percentage payment for other parties of sale.
    dsAsset ::  AssetClass, -- ^ the assetclass for cost
    dsCost:: Integer,  -- ^ total cost of asset
    dsType::  !SellType
} deriving(Show,Generic,ToJSON,FromJSON)
PlutusTx.unstableMakeIsData ''DirectSale

-- total amount received by all the seller parties
dsCostExcludingFee::Market-> DirectSale->Integer
dsCostExcludingFee  Market{mPrimarySaleFee,mSecondarySaleFee} DirectSale{dsCost,dsType} =
    (userShare * dsCost) `divide` marketHundredPercent
  where
    userShare=marketHundredPercent - case dsType  of
                                        Secondary -> mSecondarySaleFee
                                        Primary   -> mPrimarySaleFee

-- List of payment values to be payed to all the parties in directsale
dsPaymentValueList:: Market -> DirectSale -> [(PubKeyHash ,Integer)]
dsPaymentValueList market ds@DirectSale{dsParties,dsSeller} = partiesValue <> [(dsSeller,sellerValue)]
    where
        totalReceived= dsCostExcludingFee market ds
        partyShare  percent = (totalReceived  * percent ) `divide` marketHundredPercent

        partiesValue  = map (\(pkh,percent)-> (pkh,partyShare percent)) dsParties
        sellerValue  =totalReceived - (sum $ map snd partiesValue)

-- market fee to be payed when buying directsale
{-# INLINABLE dsFee #-}
dsFee :: Market -> DirectSale -> Integer
dsFee market ds@DirectSale{dsCost}=dsCost - dsCostExcludingFee market ds

data Auction = Auction{
    aOwner  :: !PubKeyHash, -- pkh Who created the auction.
    aParties:: [(PubKeyHash,Percent)], -- other parties that must be paid. the integer value is Percentage
    aBidder:: !PubKeyHash, -- Current Bidder
    aAssetClass:: !AssetClass, -- The Bidding currency for auction.
    aMinBid :: !Integer, -- starting Bid
    aMinIncrement :: !Integer, -- min increment  from previous auction per bid
    aDuration:: !POSIXTimeRange, -- Auction duration
    aValue:: Value  -- The value that's placed on Auction. this is what winner gets.
} deriving (Generic, Show,ToJSON,FromJSON)
PlutusTx.unstableMakeIsData ''Auction

-- Given final bid value
-- Calculate Value to be payed to all the seller parties of auction
-- Note that sellerReceiving = totalBid - marketFee - otherPartyPayment - buyer's Payment
{-# INLINABLE aPaymentReceiversValue #-}
aPaymentReceiversValue :: Market -> Auction -> Value-> [(PubKeyHash,Value)]
aPaymentReceiversValue m@Market{mAuctionFee} a@Auction{aOwner,aAssetClass,aValue} closingValue =
    [aOwnerPart] ++ partiesPart
  where
    aOwnerPart=(aOwner,closingValue -aValue- marketFeeValue-assetClassValue aAssetClass (sum $ map snd partiesPayment) )
    partiesPart = map (\(pkh,v)-> (pkh,assetClassValue aAssetClass v)) partiesPayment
    partiesPayment=map (\(pkh,v) ->(pkh,(v*totalPayment)`divide` marketHundredPercent)) (aParties a)
    totalPayment = aPaymentAfterFee m a closingValue
    marketFeeValue= auctionAssetValue a $ aFee  m a closingValue

-- Given final bid value in an auction,
--  Create pament map for the auction seller parties
{-# INLINABLE aPaymentRecivers #-}
aPaymentRecivers:: Market ->Auction->Value->Payment
aPaymentRecivers m@Market{mAuctionFee} a@Auction{aAssetClass,aValue,aParties} fValue=
  foldMap toPayment (aPaymentReceiversValue m a fValue)
  where
    toPayment (pkh,v) = payment pkh v

-- Given final bid value in an auction,
-- Get the amount after deducting market fee. It is to be distributed among seller parties.
{-# INLINABLE aPaymentAfterFee #-}
aPaymentAfterFee :: Market -> Auction -> Value -> Integer
aPaymentAfterFee Market{mAuctionFee} a v =totalPayment
  where
    totalPayment= ((marketHundredPercent-mAuctionFee ) * totalClosingValue) `divide` marketHundredPercent
    totalClosingValue = assetClassValueOf v (aAssetClass a)


-- Given final bid, get Market fee to be paid when claiming auction
{-# INLINABLE aFee #-}
aFee :: Market -> Auction -> Value-> Integer
aFee Market{mAuctionFee} Auction{aAssetClass} fValue = finalAssetValue - sellerValue
    where
      sellerValue=(sellerPercent * finalAssetValue) `divide` marketHundredPercent
      sellerPercent=marketHundredPercent-mAuctionFee
      finalAssetValue= assetClassValueOf fValue aAssetClass

-- the interval object representime the time by which auction can be claimed
aClaimInterval :: Auction-> Interval POSIXTime
aClaimInterval Auction{aDuration}= Interval (toLower $ ivTo aDuration) (UpperBound PosInf False)
  where
    toLower (UpperBound  a _)=LowerBound a True

-- Given a payment get It as value of Auction's asset class
{-# INLINABLE auctionAssetValue #-}
auctionAssetValue :: Auction -> Integer -> Value
auctionAssetValue Auction{aAssetClass=AssetClass (c, t)} = Ledger.Value.singleton c t


{-# INLINABLE  validateBid #-}
validateBid ::  Auction -> ScriptContext -> Bool
validateBid auction ctx@ScriptContext  {scriptContextTxInfo=info}=
  case txOutDatum  ctx newTxOut of
    Just nAuction@Auction{} ->
            traceIfFalse "Unacceptible modification to output datum" (validOutputDatum nAuction)
        &&  traceIfFalse "Only one bid per transaction" (allScriptInputsCount  ctx ==1 )
        &&  traceIfFalse "The asset is not present in the auction utxo" validInputDatum
        &&  traceIfFalse "Insufficient payment to market contract" isMarketScriptPayed
        &&  traceIfFalse "Insufficient payment to previous bidder" isExBidderPaid
        &&  traceIfFalse "Not during the auction period" duringTheValidity
        && traceIfFalse "Owner can't bid" (aBidder nAuction /= aOwner auction)
    _       -> trace "Output Datum can't be parsed to Auction" False
  where
    duringTheValidity  =   aDuration auction `contains` txInfoValidRange info
    validOutputDatum  nAuction  =  aMinIncrement auction == aMinIncrement nAuction &&
                                      aAssetClass auction == aAssetClass nAuction &&
                                      aDuration auction == aDuration nAuction &&
                                      aOwner auction== aOwner nAuction


    -- without this check, auction creator might say that
    -- they are placing asset on auction datum without locking them.
    validInputDatum = ownInputValue ctx `geq` aValue auction
    minNewBid = ownInputValue ctx <>
        auctionAssetValue auction (
            if  lastAuctionAssetValue == 0
            then  aMinBid auction
            else  aMinIncrement auction)
    isExBidderPaid= lastAuctionAssetValue == 0
      ||  (assetClassValueOf  (valuePaidTo info (aBidder auction))  (aAssetClass auction)
            >= lastAuctionAssetValue
          )

    lastAuctionAssetValue= assetClassValueOf  (ownInputValue ctx ) (aAssetClass auction)

    isMarketScriptPayed = ownOutputValue ctx `geq` minNewBid

    newTxOut=case getContinuingOutputs ctx of
       [txOut] -> txOut
       _       -> traceError "MultipleOutputs"


{-# INLINABLE  validateWithdraw #-}
validateWithdraw :: Market -> BuiltinData  -> ScriptContext -> Bool
validateWithdraw market datum ctx=
          isDirectSale
      ||  isAuction
      || traceIfFalse  "Only Operator can withdraw utxos with invalid datum" (txSignedBy info $ mOperator market)
  where
      isAuction = case fromBuiltinData  datum of
          (Just auction)      ->  traceIfFalse "Missing owner signature" (txSignedBy info (aOwner auction)) &&
                                  traceIfFalse "Cannot withdraw auction with bids" (aBidder auction==aOwner auction)
          _ -> False
      isDirectSale= case fromBuiltinData  datum of
          (Just (DirectSale dsSeller _ _ _ _ ) )   -> traceIfFalse "Missing seller signature" (txSignedBy info  dsSeller)
          _                   -> False
      info=scriptContextTxInfo ctx
      auctionNotActive auction = not $ aDuration auction `contains` txInfoValidRange info


{-# INLINABLE validateClaimAuction  #-}
validateClaimAuction :: Market  -> ScriptContext -> Bool
validateClaimAuction  market@Market{mAuctionFee,mOperator} ctx@ScriptContext{scriptContextTxInfo=info} =
          allowSingleScript ctx
      &&  traceIfFalse  "Auction not Expired" allAuctionsExpired
      &&  traceIfFalse "Market fee not paid" isOperatorPaid
      &&  traceIfFalse "Bidder not paid"     areWinnersPaid
      && traceIfFalse  "Sellers not paid"      areSellersPaid
      where
        -- auction validity
        allAuctionsExpired =  all isAuctionExpired auctionsWithTxOut
        isAuctionExpired (txOut,auction) = (ivTo  $ aDuration auction) < (ivTo $ txInfoValidRange info)

        -- Check that each of the parties are paid
        areWinnersPaid  = validatePayment (\pkh v->valuePaidTo info pkh `geq` v)  totalWinnerPayment
        areSellersPaid  = validatePayment (\pkh v -> valuePaidTo info pkh  `geq` v)  totalSellerPayment
        isOperatorPaid  = valuePaidTo info mOperator `geq`  totalOperatorFee

        -- Total payments arising from the utxos
        totalSellerPayment= foldMap  sellerPayment auctionsWithTxOut
        totalWinnerPayment= foldMap  aWinnerPayment auctionsWithTxOut
        totalOperatorFee  = foldMap  operatorFee auctionsWithTxOut

        -- payment share for each party in a auction txOut
        sellerPayment   (txOut,auction) = aPaymentRecivers market auction  (txOutValue txOut)
        operatorFee     (txOut,auction) = auctionAssetValue auction $ aFee market auction $ txOutValue txOut
        aWinnerPayment  (txOut,auction) = payment (aBidder auction) $ aValue auction

        auctionsWithTxOut:: [(TxOut,Auction)]
        auctionsWithTxOut=ownInputsWithDatum  ctx

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

        totalSellerPayment  = foldMap  sellerPartyPayment salesWithTxOut
        totalMarketFee      = foldMap  marketFeeValue salesWithTxOut

        marketFeeValue    (_,dsale)  = assetClassValue (dsAsset dsale ) $ dsFee market dsale
        sellerPartyPayment (_,dsale) = assetClassPayment (dsAsset dsale ) (dsPaymentValueList market dsale)

        salesWithTxOut:: [(TxOut,DirectSale)]
        salesWithTxOut = ownInputsWithDatum ctx


{-# INLINABLE mkMarket #-}
mkMarket :: Market ->  BuiltinData    -> MarketRedeemer -> ScriptContext  -> Bool
mkMarket market  d action ctx =
    case  action of
        Buy       -> validateBuy market ctx
        Withdraw  -> validateWithdraw market d ctx
        Bid       -> case fromBuiltinData d of
                    Just auction -> validateBid auction ctx
                    _            -> trace "Invalid Auction datum" False
        ClaimBid  -> validateClaimAuction market ctx
        _ -> trace "Invalid Redeemer dataum" False




data MarketScriptType
instance Scripts.ValidatorTypes MarketScriptType where
    type instance RedeemerType MarketScriptType = MarketRedeemer
    type instance DatumType MarketScriptType = BuiltinData


typedMarketValidator :: Market -> Scripts.TypedValidator MarketScriptType
typedMarketValidator = Scripts.mkTypedValidatorParam @MarketScriptType
    $$(PlutusTx.compile [|| mkMarket ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator

marketValidator :: Market -> Validator
marketValidator market = Scripts.validatorScript (typedMarketValidator market)


marketScript market=Scripts.validatorScript (typedMarketValidator market)

marketAddress :: Market -> Ledger.Address
marketAddress = Scripts.validatorAddress  . typedMarketValidator



-- marketValidator :: Market -> Validator
-- marketValidator market= mkValidatorScript $$(PlutusTx.compile [||a ||])
--     where
--         a _ _ _=()

-- marketAddress :: Market -> Ledger.Address
-- marketAddress = scriptAddress   . marketValidator
