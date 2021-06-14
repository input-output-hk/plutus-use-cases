{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
-- | Implements a custom currency with a monetary policy that allows
--   the forging of a fixed amount of units.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Plutus.Contract.Blockchain.MarketPlace
where

import Control.Monad ( Monad((>>), (>>=)), void )
import           GHC.Generics              (Generic)
import qualified Data.Map               as Map
import           Data.Text              (Text, length)
import           Data.Void              (Void)
import           Plutus.Contract        as Contract hiding (when)
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import PlutusTx (Data)
import qualified PlutusTx.AssocMap as AssocMap
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (singleton,fee)
import qualified PlutusTx.Prelude  as PlutusPrelude
import Ledger.Constraints as Constraints
    ( monetaryPolicy,
      otherScript,
      unspentOutputs,
      mustForgeValue,
      mustPayToOtherScript,
      mustPayToPubKey,
      mustPayToTheScript,
      mustSpendPubKeyOutput,
      mustSpendScriptOutput,
      ScriptLookups,
      TxConstraints )
import qualified Ledger.Typed.Scripts   as Scripts
import qualified Ledger.Scripts
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema, adaCurrency)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import qualified Prelude
import           Prelude                (Semigroup (..), Show (show), String)
import           Text.Printf            (printf)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8
import qualified Data.ByteString.Lazy.Internal as Lazy
import qualified Data.Sequence as Seq
import Data.String.Conversions
import Data.Aeson (FromJSON, ToJSON (toEncoding), Value (Bool), encode)
import qualified Data.Aeson.Types as Types

import  Ledger.Ada
import qualified Ledger.Ada as Ada
import Data.Semigroup (Last)
import PlutusTx.These
import qualified Data.ByteString as Builtins
import qualified Data.ByteString.UTF8 as U8
import qualified Plutus.Contract as Extras
import qualified Data.Void as Data
import Data.Aeson.Types
import qualified Data.Aeson.Extras as JSON
import Data.ByteString.Lazy (toStrict)
import Ledger.AddressMap
import qualified Data.Map
import  Data.Functor

--------------
-- Data.payment.hs
-----------------------

newtype Payment = Payment ( AssocMap.Map PubKeyHash Ledger.Value )
    deriving stock (Generic)
    deriving anyclass (ToJSON, FromJSON)
    deriving Show



instance PlutusPrelude.Semigroup Payment where
    {-# INLINABLE (<>) #-}
    (<>) = combinePayments

{-# INLINABLE combinePayments #-}
combinePayments ::  Payment -> Payment -> Payment
combinePayments  (Payment a) (Payment b) = Payment (a PlutusPrelude.<> b)

{-# INLINABLE foldPaymnents #-}
foldPaymnents :: [Payment] ->Payment
foldPaymnents = PlutusPrelude.foldl (PlutusPrelude.<>) emptyPayment

{-# INLINABLE payment  #-}
payment :: PubKeyHash -> Ledger.Value -> Payment
payment pkHash value=Payment  (AssocMap.singleton pkHash value)

{-# INLINABLE lovelacePayment #-}
lovelacePayment:: PubKeyHash -> Integer ->Payment
lovelacePayment pkh lovelace=payment pkh (lovelaceValueOf lovelace)


{-# INLINABLE hasLovelace  #-}
hasLovelace::Payment -> PubKeyHash -> Integer -> Bool
hasLovelace (Payment p) pkh pay = case AssocMap.lookup pkh p of
        Nothing -> False
        Just x -> valueOf x adaSymbol  adaToken >=pay

{-# INLINABLE paymentPkhs #-}
paymentPkhs :: Payment -> [PubKeyHash]
paymentPkhs (Payment x) =  AssocMap.keys x


{-# INLINABLE emptyPayment  #-}
emptyPayment ::Payment
emptyPayment = Payment AssocMap.empty


-------------------
-- Marketplace.hs
----------------


data Market = Market
    {   mOperator           :: !PubKeyHash
    ,   mPrimarySaleFee     :: !Integer
    ,   mSecondarySaleFee   :: !Integer
    ,   mAuctionFee         :: !Integer
    } deriving (Show,Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''Market


data MarketRedeemer =  ClaimBid| Bid | Buy | TakeBack | Collect deriving (FromJSON,ToJSON,Show,Generic)

PlutusTx.makeLift ''MarketRedeemer
PlutusTx.unstableMakeIsData ''MarketRedeemer

data SellType = Primary | Secondary  deriving (Generic,ToJSON,FromJSON)

PlutusTx.unstableMakeIsData ''SellType
PlutusTx.makeLift ''SellType

data DirectSale=DirectSale{
    dsSeller::PubKeyHash,
    -- flattened value
    dsCost :: [(CurrencySymbol, TokenName, Integer)],
    dsType:: SellType
} deriving(Generic,ToJSON,FromJSON)
PlutusTx.unstableMakeIsData ''DirectSale
PlutusTx.makeLift ''DirectSale


data Auction = Auction{
    aBidder::PubKeyHash,
    aAssetClass:: AssetClass,
    aMinBid :: Integer,
    aMinIncrement :: Integer,
    aDuration::POSIXTimeRange
} deriving (Generic, ToJSON,FromJSON)

auctionAssetValue Auction{aAssetClass=AssetClass (c, t)} = Value.singleton c t
auctionAssetValueOf Auction{aAssetClass} value = assetClassValueOf value aAssetClass

PlutusTx.unstableMakeIsData ''Auction
PlutusTx.makeLift ''Auction


-- marketUtxoData::forall w s e. (HasOwnPubKey  s,AsContractError e )=>Integer  -> Contract w s e MarketUtxoData
-- marketUtxoData cost=do
--         pk<-  ownPubKey
--         pure $ MarketUtxoData (Just (cost,pubKeyHash pk))

-- {-# INLINABLE marketUtxoCost #-}
-- marketUtxoCost :: MarketUtxoData -> Ledger.Value
-- marketUtxoCost (MarketUtxoData (Just (cost,_))) = lovelaceValueOf  cost

-- maybeMarketUtxoData :: TxOutTx -> Maybe MarketUtxoData
-- maybeMarketUtxoData o =marketData (txOutTxOut o) $ \dh -> Map.lookup dh $ txData $ txOutTxTx o
--     where
--     marketData :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe MarketUtxoData
--     marketData o f = do
--         dh      <- txOutDatum o
--         Datum d <- f dh
--         PlutusTx.fromData d

-- {-# INLINABLE emptyMarketUtxoData  #-}
-- emptyMarketUtxoData :: MarketUtxoData
-- emptyMarketUtxoData=MarketUtxoData Nothing
ownInputs:: ScriptContext -> [TxOut]
ownInputs ctx@ScriptContext{scriptContextTxInfo=TxInfo{txInfoInputs}}=
     filter (\x->txOutAddress x==ownAddress) resolved
    where
    resolved=map (\x->txInInfoResolved x) txInfoInputs
    ownAddress=scriptHashAddress (ownHash ctx)

ownInputDatum :: ScriptContext -> Maybe a
ownInputDatum ctx = do
    txInfo <-findOwnInput ctx
    let txOut= txInInfoResolved txInfo
    outputDatum ctx txOut

outputDatum:: ScriptContext ->TxOut -> Maybe a
outputDatum ctx txOut =do
            dHash<-txOutDatumHash txOut
            datum<-findDatum dHash (scriptContextTxInfo ctx)
            PlutusTx.fromData (getDatum datum)

ownInputValue:: ScriptContext -> Value.Value
ownInputValue ctx = case  findOwnInput ctx of
      Just TxInInfo{txInInfoResolved} ->  txOutValue txInInfoResolved

ownOutputValue :: ScriptContext -> Value.Value
ownOutputValue ctx = valueLockedBy (scriptContextTxInfo ctx) (ownHash ctx)


validateBid :: Market -> Auction -> ScriptContext -> Bool
validateBid market auction ctx@ScriptContext  {scriptContextTxInfo=info}= let
        hasSingleUtxo=    PlutusPrelude.length ownInputs == 1
        duringTheValidity  =   aDuration auction `contains` txInfoValidRange info
        validOutputDatum    =   case getContinuingOutputs ctx of
            [txOut] -> case outputDatum ctx txOut of
                    Just nAuction@Auction{} ->  aMinIncrement auction == aMinIncrement nAuction &&
                                                aAssetClass auction == aAssetClass nAuction &&
                                                aDuration auction == aDuration nAuction
                    _                       -> traceError "Invalid Output datum"
            _       -> traceError "Multiple outputs"
        minNewBid =  (ownInputValue ctx ) <> case assetClassValueOf  (ownInputValue ctx ) (aAssetClass auction) of
                        0  -> auctionAssetValue auction $ aMinBid auction
                        _  -> auctionAssetValue auction $ aMinIncrement auction

        isExBidderPaid= case assetClassValueOf  (ownInputValue ctx ) (aAssetClass auction) of
                    0 -> True
                    v -> assetClassValueOf  (valuePaidTo info (aBidder auction))  (aAssetClass auction) >= v

        isMarketScriptPayed tx nAuction= (ownOutputValue ctx ) >= minNewBid

        doValidate txOut newAuction=
                traceIfFalse "Only one bid per transaction" hasSingleUtxo
            &&  traceIfFalse "Insufficient payment to market contract" ( isMarketScriptPayed txOut newAuction)
            &&  traceIfFalse "Insufficient payment to previous bidder" isExBidderPaid
            &&  traceIfFalse "Not during the auction period" duringTheValidity
            &&  traceIfFalse "Unacceptible modification to output datum" validOutputDatum


    in  (case getContinuingOutputs ctx of
            [txOut] -> case outputDatum ctx txOut of
                    Just nAuction@Auction{} -> doValidate txOut nAuction
            _       -> traceError "Multiple outputs"
    )

validateTakeback datum ctx= isAuction || isDirectSale

    where
        isAuction = case PlutusTx.fromData datum of
            (Just auction)      -> (txSignedBy info (aBidder auction)) &&
                                     ( auctionNotActive auction)
            _ -> False
        isDirectSale= case PlutusTx.fromData  datum of
            (Just directSale)   -> txSignedBy info $ dsSeller directSale
            _                   -> False
        info=scriptContextTxInfo ctx
        auctionNotActive auction = not $ aDuration auction `contains` txInfoValidRange info

validateClaimAuction  Market{mAuctionFee} datum ctx@ScriptContext{scriptContextTxInfo=info} =
    case PlutusTx.fromData datum of
        Just auction ->
                traceIfFalse "Missing bidder signature" (hasBidderSignature || isBidderPaid)
            &&  traceIfTrue  "Auction not Completed"isAuctionPeriod
            &&  traceIfFalse "Market fee not paid" isMarketPaid
            where
                hasBidderSignature = txSignedBy info (aBidder auction)
                isBidderPaid = foldPaymnents  $ mapMaybe txOutToPayment  (ownInputs ctx)
                isAuctionPeriod auction= not $ aDuration auction `contains` txInfoValidRange info
                isMarketPaid = valuePaidTo info (mOperator market)

                txOutToPayment txOut@TxOut{txOutValue ,txOutAddress} =do
                        auction<-outputDatum ctx txOut
                        if isAuctionPeriod auction then Nothing else (do
                            pkh<-toPubKeyHash txOutAddress
                            fee <-marketExpectation txOut
                            pure $ payment pkh (txOutValue - fee))


                marketExpectation txOut@TxOut{txOutValue} =do
                        auction <- outputDatum ctx txOut
                        let aClass=aAssetClass auction
                        pure $ assetClassValue aClass (marketShare (assetClassValueOf txOutValue aClass))

                marketShare x = x - userShare x
                userShare x =x*(1000000-x) `Prelude.div` 1000000


        _       ->
            traceError "Invalid input datum"



{-# INLINABLE mkMarket #-}
mkMarket :: Market -> Data  -> MarketRedeemer -> ScriptContext -> Bool
mkMarket market _ action ctx@ScriptContext{scriptContextTxInfo=info@TxInfo{txInfoInputs=inputs, txInfoOutputs=outputs},scriptContextPurpose=Spending txoutRef} =
    case action of
        Buy       ->    traceIfFalse "Insufficient payment" allSellersPaid
                    &&  traceIfFalse "Insufficient fees" isMarketPayed
        Bid       -> bid
        ClaimBid  -> False
        Collect   -> False
        TakeBack -> canTakeback
    where

        canTakeback= all (\x -> requireSellerSignature x) marketInputs

        marketInputs::[TxOut]
        marketInputs = filter (\x->txOutAddress x==ownAddress) (map (\x->txInInfoResolved x) inputs)

        allSellersPaid::Bool
        allSellersPaid = foldl (\truth pkh ->truth && isSellerPaid pkh) True  (paymentPkhs totalPayment)

        isSellerPaid:: PubKeyHash ->Bool
        isSellerPaid pkh=hasLovelace totalPayment pkh $ sellerShare $ valueOf  (valuePaidTo info pkh) adaSymbol adaToken

        isMarketPayed::  Bool
        isMarketPayed= (valueOf  (valuePaidTo info operator) adaSymbol adaToken )
                             >=
                        (sum  $ map (\a->marketExpectation a) marketInputs)

        totalPayment :: Payment
        totalPayment =foldPaymnents $ map (\a->paymentInfo a) marketInputs

        paymentInfo :: TxOut  ->  Payment
        paymentInfo  txOut = case findCost txOut  of
            Just (value,hash) ->  lovelacePayment hash value
            _                 -> traceError "No Cost info"

        marketExpectation :: TxOut  -> Integer
        marketExpectation txOut = case findCost txOut of
            Just (value,hash) ->  value
            _                 -> traceError "No Cost info"

        findCost :: TxOut ->Maybe (Integer,PubKeyHash)
        findCost txOut= do
            dHash<-txOutDatumHash txOut
            datum<-findDatum dHash info
            marketUtxoData <-PlutusTx.fromData (getDatum datum)
            unmarketUtxoData marketUtxoData

        requireSellerSignature txOut =case findCost txOut
            of
                Just (_, pkh) -> traceIfFalse   "Missing Seller signature"  (txSignedBy info pkh)
                _             ->  traceIfFalse  "Utxo has no owher info but has value" False

        sellerShare x= x * (10000000000 - fee) `Prelude.div` 10000000000
        marketShare x= x -  sellerShare x



-- data MarketType
-- instance Scripts.ScriptType MarketType where
--     type instance DatumType MarketType = MarketUtxoData
--     type instance RedeemerType MarketType =  MarketAction

-- marketScript :: Market -> Scripts.ScriptInstance MarketType
-- marketScript market = Scripts.validator @MarketType
--     ($$(PlutusTx.compile [|| mkMarket ||]) `PlutusTx.applyCode` PlutusTx.liftCode market)
--     $$(PlutusTx.compile [|| wrap ||])
--     where
--         wrap = Scripts.wrapValidator @MarketUtxoData @MarketAction

-- marketValidator :: Market -> Validator
-- marketValidator = Scripts.validatorScript . marketScript

-- marketAddress :: Market -> Ledger.Address
-- marketAddress = scriptAddress . marketValidator

-- moveToMarket' :: forall s e. ( HasBlockchainActions s,
--                                 AsContractError e )
--         =>Market  -> AssetClass ->Integer
--              -> Contract [Data.Aeson.Types.Value] s e Data.Aeson.Types.Value
-- moveToMarket' market@Market{operator=operator,fee=fee} asset price= do
--     utxoData <- marketUtxoData price
--     ledgerTx <- submitTxConstraints  inst (Constraints.mustPayToTheScript utxoData value)
--     awaitTxConfirmed ( txId ledgerTx)
--     tell [toJSON  @String "Moved to market"]
--     pure Types.Null
--     -- return $ txId ledgeTx
--     where
--         value  = assetClassValue asset 1
--         inst = marketScript market

-- moveToMarket :: forall w s e. (HasBlockchainActions s,AsContractError e )
--      =>Market  -> SellParams -> Contract [Data.Aeson.Types.Value] s e Data.Aeson.Types.Value
-- moveToMarket market sp=
--     moveToMarket' market (spAsset sp) (spCost sp)

-- buyFromMarket :: forall w s. HasBlockchainActions s => Market -> AssetId -> Contract [Data.Aeson.Types.Value] s Text Data.Aeson.Types.Value
-- buyFromMarket market asset = do
--     logInfo @String $ "requested buy"
--     utxoInfo <- findInMarket market  $ AssetClass  (CurrencySymbol (assCurrency asset) ,TokenName ( convertString (assToken asset)))
--     case utxoInfo of
--         Just (oref, o,  (cost,seller)) -> do
--             let tx = Constraints.mustSpendScriptOutput oref (Redeemer ( PlutusTx.toData Buy))
--                     <> Constraints.mustPayToPubKey  seller (lovelaceValueOf  cost)
--                     -- <> Constraints.mustPayToOtherScript (validatorHash $ marketValidator market)  (Ledger.Scripts.Datum $ PlutusTx.List [] ) (Ada.lovelaceValueOf  2) 
--                 lookups = Constraints.unspentOutputs (Map.singleton oref o)     <>
--                           Constraints.otherScript  (marketValidator market) 
--             ledgerTx <- submitTxConstraintsWith @MarketType lookups tx
--             awaitTxConfirmed $ txId ledgerTx
--             pure $ toJSON @String $ "Bought asset "++ show asset ++ "at the cost of " ++ show  cost ++" Lovelace"
--         _ -> do
--             logError  @String $ "It was either not on sale or is not on sale anymore " ++ show asset
--             pure $  toJSON  @String $ "Asset not found in market"



-- findInMarket :: forall w s. HasBlockchainActions s =>
--              Market -> AssetClass
--              -> Contract w s Text (Maybe (TxOutRef, TxOutTx, (Integer,PubKeyHash)))
-- findInMarket market asset = do
--     utxos <- Map.filter hasAsset PlutusPrelude.<$> utxoAt (marketAddress market)
--     pure $ case Map.toList utxos of
--         [(oref, o)] -> case maybeMarketUtxoData o of
--             Just d@MarketUtxoData{unmarketUtxoData=(Just a)} -> Just (oref,o,a)
--             _           -> Nothing
--         _           -> Nothing
--   where
--     cost o = do
--         x<-maybeMarketUtxoData o
--         unmarketUtxoData x

--     hasAsset :: TxOutTx -> Bool
--     hasAsset o = assetClassValueOf (txOutValue $ txOutTxOut o) asset == 1


-- data ValueInfo=ValueInfo{
--     currency::ByteString,
--     token:: String,
--     value:: Integer
-- } deriving(Generic,FromJSON,ToJSON,Prelude.Show)

-- data NftsOnSaleResponse=NftsOnSaleResponse{
--     cost::Integer,
--     owner:: ByteString,
--     values:: [ValueInfo]
-- }deriving(Generic,FromJSON,ToJSON,Prelude.Show)

-- toValueInfo::Ledger.Value ->[ValueInfo]
-- toValueInfo v=map doMap $ flattenValue v
--     where
--         doMap (c,t,v)=ValueInfo (unCurrencySymbol c) (convertString (unTokenName t)) v

-- utxosOnSale :: forall w s. HasBlockchainActions s =>
--              Market  -> Contract w s Text (Map.Map TxOutRef  (TxOutTx, MarketUtxoData))
-- utxosOnSale market=do 
--     utxos<-utxoAt (marketAddress market)
--     let responses =  filterSales utxos
--     return responses
--     where
--         filterSales:: UtxoMap ->Data.Map.Map TxOutRef  (TxOutTx, MarketUtxoData)
--         filterSales m =Map.mapMaybeWithKey  doMap m

--         doMap:: TxOutRef -> TxOutTx  ->Maybe (TxOutTx, MarketUtxoData)
--         doMap  txOutRef txOutTx =case maybeMarketUtxoData txOutTx of
--             Just x-> Just (txOutTx,x)
--             _ -> Nothing


-- nftsOnSale :: forall w s. HasBlockchainActions s =>
--              Market  -> Contract w s Text [NftsOnSaleResponse]
-- nftsOnSale market = do
--         utxos <- utxosOnSale market
--         pure $ map doMap  $ Map.elems utxos
--     where
--         doMap:: (TxOutTx,MarketUtxoData) -> NftsOnSaleResponse
--         doMap (TxOutTx _ txOut,MarketUtxoData{unmarketUtxoData=Just (c,pkh)})= NftsOnSaleResponse c ( getPubKeyHash pkh) (toValueInfo (txOutValue txOut))


-- myUtxosOnSale :: forall w s. HasBlockchainActions s =>
--              Market ->PubKeyHash -> Contract w s Text (Map.Map TxOutRef  (TxOutTx, MarketUtxoData))
-- myUtxosOnSale market  pubKeyHash=do
--     onsale <- utxosOnSale market
--     pure $ Map.filter (belongsTo pubKeyHash) onsale
--     where 
--         belongsTo :: PubKeyHash -> (TxOutTx,MarketUtxoData)  -> Bool
--         belongsTo pk utxoData@(a,utxo)= isOwner pk utxo

--         isOwner :: PubKeyHash ->MarketUtxoData ->Bool
--         isOwner _pk MarketUtxoData{unmarketUtxoData=(Just (_,pk))}= _pk==pk
--         isOwner _ _ = False

-- myNftsOnSale :: forall w s. HasBlockchainActions s =>
--              Market  ->PubKeyHash-> Contract w s Text [NftsOnSaleResponse]
-- myNftsOnSale market pubKeyHash = do
--         utxos <- myUtxosOnSale market pubKeyHash
--         pure $ map doMap  $ Map.elems utxos
--     where
--         doMap:: (TxOutTx,MarketUtxoData) -> NftsOnSaleResponse
--         doMap (TxOutTx _ txOut,MarketUtxoData{unmarketUtxoData=Just (c,pkh)})= NftsOnSaleResponse c (getPubKeyHash pkh) (toValueInfo (txOutValue txOut))

-- type MarketSchema =
--     BlockchainActions
--         .\/ Endpoint "sell" SellParams
-- --       .\/ Endpoint "withdraw" (AssetClass)
--         .\/ Endpoint "buy"  AssetId
--         .\/ Endpoint "funds" String
--         .\/ Endpoint "mint"  String
--         .\/ Endpoint "onsale" String  
--         .\/ Endpoint "myonsale" String

-- mkSchemaDefinitions ''MarketSchema



-- endpoints :: Market -> Contract [Types.Value] MarketSchema Text  ()
-- endpoints market =  handled >> endpoints market
--   where
--     handled =handleError handler (selections >> pure ())
--     selections=moveToMarket''
--         `select`buyFromMarket''
--         `select` ownFunds''
--         `select` mintEp''
--         `select` onsale''
--         `select` myonsale''
--     buyFromMarket''= (endpoint @"buy")  >>=buyFromMarket market
--     moveToMarket''=  (endpoint @"sell") >>= moveToMarket market
--     ownFunds'' = (endpoint @"funds") >> ownFunds
--     mintEp'' = (endpoint @"mint") >>= (\x -> pure (TokenName (convertString x)))>>= mintEp
--     onsale'' = (endpoint @"onsale") >> nftsOnSaleEp market
--     myonsale''= (endpoint @"myonsale") >> myNftsOnSaleEp market
--     handler e = do
--         Contract.logError $ show e


-- myNftsOnSaleEp :: (HasBlockchainActions s) => Market-> Contract [Types.Value ] s Text  Types.Value
-- myNftsOnSaleEp m=do
--     pk<-ownPubKey 
--     d<-myNftsOnSale m (pubKeyHash pk)
--     let ret=toJSON d
--     tell [ret]
--     return ret

-- nftsOnSaleEp :: (HasBlockchainActions s) => Market-> Contract [Types.Value ] s Text  Types.Value
-- nftsOnSaleEp m=do
--     pk<-ownPubKey 
--     d <- nftsOnSale m
--     let ret=toJSON d
--     tell [ret]
--     return ret


-- instance ToJSON Lazy.ByteString  where
--     toEncoding  bs =
--          toEncoding $ Types.String  (doConvert bs)

--     toJSON bs = Types.String (doConvert bs)



-- doConvert::Lazy.ByteString ->Text
-- doConvert=convertString


-- --Utility to get Own funds
-- --
-- -- 
-- --

-- ownFunds:: (HasBlockchainActions s) => Contract [Types.Value ] s Text  Data.Aeson.Types.Value
-- ownFunds = do
--     pk    <- ownPubKey
--     utxos <- utxoAt $ pubKeyAddress pk
--     let v = mconcat $ Map.elems $ txOutValue . txOutTxOut Prelude.<$> utxos
--     logInfo @String $ "own funds: " ++ show (Value.flattenValue v)
--     tell $ [ toJSON v]
-- -- let's hope that in future we can return the json string without having to tell
--     return $ toJSON  v


-- ---NFT Token Part Below
-- ---
-- ---

-- {-# INLINABLE mkPolicy #-}
-- mkPolicy :: TxOutRef -> TokenName -> ScriptContext -> Bool
-- mkPolicy oref tn ctx@ScriptContext {scriptContextTxInfo=info@TxInfo{}} =
--     traceIfFalse "UTxO not consumed"   hasUTxO           &&
--     traceIfFalse "wrong amount minted" checkMintedAmount
--     where

--     hasUTxO :: Bool
--     hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

--     checkMintedAmount :: Bool
--     checkMintedAmount = case flattenValue (txInfoForge info) of
--         [(cs, tn', amt)] -> cs  == ownCurrencySymbol ctx && tn' == tn && amt == 1
--         _                -> False

-- policy :: TxOutRef -> TokenName -> Scripts.MonetaryPolicy
-- policy oref tn = mkMonetaryPolicyScript $
--     $$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMonetaryPolicy $ mkPolicy oref' tn' ||])
--     `PlutusTx.applyCode`
--     PlutusTx.liftCode oref
--     `PlutusTx.applyCode`
--     PlutusTx.liftCode tn

-- curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
-- curSymbol oref tn = scriptCurrencySymbol $ policy oref tn



-- mint ::  (HasBlockchainActions s) =>
--     TokenName -> Contract w s Text (Maybe (ScriptLookups a, TxConstraints i o))
-- mint tn = do
--     pk    <- Contract.ownPubKey
--     utxos <- utxoAt (pubKeyAddress pk)
--     case Map.keys utxos of
--         []       -> Contract.logError @String "no utxo found" >> pure Nothing
--         oref : _ -> do
--             let val     = Value.singleton (curSymbol oref tn) tn 1
--                 lookups = Constraints.monetaryPolicy (policy oref tn) <> Constraints.unspentOutputs utxos
--                 tx      = Constraints.mustForgeValue val <> Constraints.mustSpendPubKeyOutput oref
--             pure $ Just (lookups,tx)

-- mintEp ::  (HasBlockchainActions s) =>TokenName -> Contract [Types.Value ] s Text Types.Value
-- mintEp tn=do
--     pk    <- Contract.ownPubKey
--     utxos <- utxoAt (pubKeyAddress pk)
--     case Map.keys utxos of
--         []       -> Contract.logError @String "no utxo found" >> pure Types.Null
--         oref : _ -> do
--             let val     = Value.singleton (curSymbol oref tn) tn 1
--                 lookups = Constraints.monetaryPolicy (policy oref tn) <> Constraints.unspentOutputs utxos
--                 tx      = Constraints.mustForgeValue val <> Constraints.mustSpendPubKeyOutput oref
--             ledgerTx <- submitTxConstraintsWith @Void lookups tx
--             void $ awaitTxConfirmed $ txId ledgerTx
--             tell  [toJSON ( unCurrencySymbol (curSymbol oref tn), convertString (unTokenName tn)::String)]
--             Contract.logInfo @String $ printf "forged %s" (show val)
--             pure $ toJSON (curSymbol oref tn,unTokenName  tn )


-- ------------------
-- -- Market Endpoints.hs
-- ------------------------

-- data ResponseTypes = Funds| MarketNfts|OwnNftsOnSale |Minted|PlacedOnMarket|Bought deriving(Generic,Prelude.Eq,Prelude.Show,ToJSON,FromJSON)
-- data ApiResponse t  = APISequence {
--         sequence :: Integer,
--         contentType::ResponseTypes,
--         content :: t
--     }
--     deriving (Generic,Prelude.Eq ,Prelude.Show,ToJSON,FromJSON)

-- data AssetId=AssetId
--     {
--         assCurrency :: !ByteString,
--         assToken:: String
--     } deriving(Generic, ToJSON,FromJSON,Prelude.Show,ToSchema )


-- data MintParams = MintParams
--     { mpTokenName :: !TokenName
--     , mpAmount    :: !Integer
--     } deriving (GHC.Generics.Generic , ToJSON, FromJSON, ToSchema)

-- data SellParams =SellParams
--     {
--         spCurrency        :: !ByteString ,
--         spToken           :: !String,
--         spCost   :: !Integer
--     } deriving(GHC.Generics.Generic ,ToJSON,FromJSON,ToSchema)
