{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Plutus.Contract.Wallet.EndpointModels
where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON (toJSON), FromJSON, object, (.=))
import Data.ByteString hiding (map)
import Playground.Contract
import Plutus.Contract.Blockchain.MarketPlace
import Ledger hiding (value,singleton,fee)
import Ledger.Value
import Data.String.Conversions (convertString)
import Ledger.Ada (adaSymbol,adaToken)
import Data.Aeson.Extras
import Data.ByteString.Builder
import Plutus.Contract.Wallet.Utils (ParsedUtxo)
import Ledger.TimeSlot (slotToPOSIXTime)
import Wallet.Emulator.Wallet (walletPubKey)


-- The models in this package are the response types used to tell state.
-- These data classes have much flatter structure compared to the default JSON types of 
-- the classes.
-- For example response for Asset class by default is
-- {
--   "unAssetClass":[{"unCurrencySymbol": "abcd...",{"unTokenName":"0xabc..."}}]
-- }
-- but with AssetId, the response is 
--{
--  "assCurrency":"abcd...",
--  "assToken": "0xabcd..."
--}
-- It's much readable and is easier to understant in  frontend.



-- represents an AssetClass
data AssetId=AssetId
    {
        assCurrency :: !ByteString,
        assToken:: !ByteString
    } deriving(Generic, ToJSON,FromJSON,Prelude.Show,ToSchema )

data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmount    :: !Integer
    } deriving (GHC.Generics.Generic , ToJSON, FromJSON, ToSchema)

data SellParams =SellParams
    {
        spItems::[ValueInfo],
        spSaleType :: SellType, -- Primary | Secondary
        spCost::ValueInfo
    } deriving(GHC.Generics.Generic,ToJSON ,FromJSON,ToSchema,Show)

-- Singleton of a Value
data ValueInfo=ValueInfo{
    currency::ByteString,
    token:: ByteString,
    value:: Integer
} deriving(Generic,FromJSON,Prelude.Show,ToSchema,Prelude.Eq )

defaultSellparam= SellParams{
    spItems=[ValueInfo "abcd" "defg" 32],
    spSaleType= Primary,
    spCost= ValueInfo "degf" "abde" 23
}
instance ToJSON ValueInfo
  where
    toJSON (ValueInfo c t v) = object [  "currency" .= doConvert c, "token" .= doConvert t,"value".=toJSON v]
      where

        doConvert bs= toJSON $ toText bs

        toText bs= encodeByteString bs

data PurchaseParam =PurchaseParam
  {
    ppValue:: ValueInfo,
    ppItems:: [TxOutRef]
  } deriving(GHC.Generics.Generic,ToJSON,FromJSON,ToSchema)

data AuctionParam = AuctionParam{
    apValue::[ValueInfo],
    apMinBid:: ValueInfo,
    apMinIncrement:: Integer,
    apStartTime::POSIXTime,
    apEndTime::POSIXTime
} deriving(GHC.Generics.Generic,ToJSON,FromJSON,ToSchema)

instance ToSchema POSIXTime
data BidParam=BidParam{
  ref :: TxOutRef,
  bidValue       :: [ValueInfo]
} deriving(Generic,ToJSON,FromJSON,ToSchema)

data ClaimParam=ClaimParam{
  references ::[TxOutRef],
  ignoreUnClaimable :: Bool
} deriving(Generic,ToJSON,FromJSON,ToSchema)
instance ToSchema TxId
instance ToSchema TxOutRef

data Bidder = Bidder{
      bPubKeyHash :: PubKeyHash,
      bBid  :: Integer,
      bBidReference:: TxOutRef
} deriving (Generic,FromJSON,ToJSON,Prelude.Show,Prelude.Eq)


data AuctionResponse = AuctionResponse{
      arOwner :: PubKeyHash,
      arValue ::[ValueInfo],
      arMinBid:: ValueInfo,
      arMinIncrement:: Integer,
      arDuration::(Extended  POSIXTime,Extended  POSIXTime),
      arBidder :: Bidder,
      arMarketFee:: Integer
}deriving (Generic,FromJSON,ToJSON,Prelude.Show,Prelude.Eq)


data NftsOnSaleResponse=NftsOnSaleResponse{
    cost::ValueInfo ,
    saleType:: SellType,
    fee:: Integer,
    owner:: ByteString,
    values:: [ValueInfo],
    reference :: TxOutRef
}deriving(Generic,FromJSON,ToJSON,Prelude.Show,Prelude.Eq)

data MarketType=MtDirectSale | MtAuction  deriving (Show, Prelude.Eq,Generic,ToJSON,FromJSON,ToSchema)


data ListMarketRequest  = ListMarketRequest{
    lmUtxoType::MarketType,
    lmByPkHash:: Maybe ByteString,
    lmOwnPkHash:: Maybe Bool
} deriving (Show, Prelude.Eq,Generic,ToJSON,FromJSON,ToSchema)


assetIdToAssetClass :: AssetId -> AssetClass
assetIdToAssetClass AssetId{assCurrency,assToken}=AssetClass (CurrencySymbol assCurrency, TokenName $ convertString assToken )

assetIdOf:: AssetClass -> AssetId
assetIdOf (AssetClass (CurrencySymbol c, TokenName t))=AssetId{
    assCurrency = c,
    assToken=t
  }

sellParamToDirectSale :: PubKeyHash -> SellParams->DirectSale
sellParamToDirectSale  pkh (SellParams items stype cost) = DirectSale {
                      dsSeller= pkh,
                      dsCost = valueInfoToPrice cost,
                      dsType=stype
                      }

aParamToAuction :: PubKeyHash -> AuctionParam -> Auction
aParamToAuction ownerPkh ap  =Auction {
              aOwner        =  ownerPkh,
              aBidder       = ownerPkh,
              aAssetClass   = valueInfoAssetClass (apMinBid  ap),
              aMinBid       = value ( apMinBid  ap),
              aMinIncrement = apMinIncrement  ap,
              aDuration     =  Interval ( LowerBound  (Finite $ apStartTime ap) True) ( UpperBound  (Finite $ apEndTime ap) False),
              aValue        = mconcat $ map valueInfoToValue ( apValue  ap)
          }


directSaleToResponse:: Market -> ParsedUtxo DirectSale  -> NftsOnSaleResponse
directSaleToResponse market (txOutRef,txOutTx,DirectSale{dsCost,dsSeller,dsType}) =
        NftsOnSaleResponse{
            cost=priceToValueInfo dsCost ,
            saleType= dsType,
            fee= if dsType == Secondary  then mPrimarySaleFee  market else mSecondarySaleFee market,
            owner= getPubKeyHash dsSeller,
            values= toValueInfo (txOutValue (txOutTxOut  txOutTx)),
            reference=txOutRef
        }

auctionToResponse:: Market -> ParsedUtxo Auction -> AuctionResponse
auctionToResponse market  (ref,TxOutTx tx (TxOut addr value _ ), a) = AuctionResponse{
      arOwner = aOwner a,
      arValue = toValueInfo (aValue a),
      arMinBid = valueInfo (aAssetClass a) (aMinBid a),
      arMinIncrement = aMinIncrement a,
      arDuration =  (lb (ivFrom   (aDuration a)),ub (ivTo (aDuration a))),
      arBidder = Bidder{
                  bPubKeyHash   = aBidder  a,
                  bBid          = auctionAssetValueOf a value,
                  bBidReference =  ref
              },
      arMarketFee = mAuctionFee market
}
  where
    lb (LowerBound a _ )=a
    ub (UpperBound a _) =a

valueInfoLovelace :: Integer -> ValueInfo
valueInfoLovelace=ValueInfo (unCurrencySymbol adaSymbol) (unTokenName adaToken)

valueInfoAssetClass:: ValueInfo -> AssetClass
valueInfoAssetClass (ValueInfo c t _)= AssetClass (CurrencySymbol c, TokenName t)

toValueInfo::Value ->[ValueInfo]
toValueInfo v=map doMap $ flattenValue v
    where
        doMap (c,t,v)=ValueInfo (unCurrencySymbol c) ( unTokenName t) v

valueInfoToPrice :: ValueInfo -> Price
valueInfoToPrice ValueInfo{currency,token,value}= Price  (CurrencySymbol currency, TokenName token, value)

valueInfoToValue ::ValueInfo -> Value
valueInfoToValue ValueInfo{currency,token,value}= Ledger.Value.singleton (CurrencySymbol currency) (TokenName  token) value

valueInfosToValue :: [ValueInfo] -> Value
valueInfosToValue vinfos= mconcat $ map valueInfoToValue vinfos

valueInfo :: AssetClass  -> Integer -> ValueInfo
valueInfo (AssetClass (c, t)) = ValueInfo (unCurrencySymbol c) (convertString (unTokenName t))

priceToValueInfo::Price ->ValueInfo
priceToValueInfo (Price (c, t, v))=ValueInfo (unCurrencySymbol c) (convertString (unTokenName t)) v