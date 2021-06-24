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


data ResponseTypes = Funds| MarketNfts|OwnNftsOnSale |Minted|PlacedOnMarket|Bought deriving(Generic,Prelude.Eq,Prelude.Show,ToJSON,FromJSON)
data ApiResponse t  = APISequence {
        sequence :: Integer,
        contentType::ResponseTypes,
        content :: t
    }
    deriving (Generic,Prelude.Eq ,Prelude.Show,ToJSON,FromJSON)

data AssetId=AssetId
    {
        assCurrency :: !ByteString,
        assToken:: !ByteString
    } deriving(Generic, ToJSON,FromJSON,Prelude.Show,ToSchema )

assetIdToAssetClass :: AssetId -> AssetClass
assetIdToAssetClass AssetId{assCurrency,assToken}=AssetClass (CurrencySymbol assCurrency, TokenName $ convertString assToken )

assetIdOf:: AssetClass -> AssetId
assetIdOf (AssetClass (CurrencySymbol c, TokenName t))=AssetId{
    assCurrency = c,
    assToken=t
  }

data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmount    :: !Integer
    } deriving (GHC.Generics.Generic , ToJSON, FromJSON, ToSchema)

data SellParams =SellParams
    {
        spItems::[ValueInfo],
        spSaleType :: SellType,
        spCost::ValueInfo
    } deriving(GHC.Generics.Generic,ToJSON ,FromJSON,ToSchema,Show)

data PurchaseParam =PurchaseParam
  {
    ppItems:: [TxOutRef],
    ppValue:: ValueInfo
  } deriving(GHC.Generics.Generic,ToJSON,FromJSON,ToSchema)

data AuctionParam = AuctionParam{
    apValue::[ValueInfo],
    apMinBid:: ValueInfo,
    apMinIncrement:: Integer,
    apStartTime::POSIXTime,
    apEndTime::POSIXTime
} deriving(GHC.Generics.Generic,ToJSON,FromJSON,ToSchema)


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
instance ToSchema POSIXTime 

data BidParam=BidParam{
  ref :: TxOutRef,
  bidValue       :: [ValueInfo]
} deriving(Generic,ToJSON,FromJSON,ToSchema)

data ClaimParam=ClaimParam{
  refreences ::[TxOutRef],
  ignoreUnClaimable :: Bool
} deriving(Generic,ToJSON,FromJSON,ToSchema)
instance ToSchema TxId
instance ToSchema TxOutRef

dummySellParam ::  SellParams
dummySellParam =SellParams{
                spItems=toValueInfo $ Ledger.Value.singleton (CurrencySymbol "ab") (TokenName "cd") 1,
                spSaleType = Primary  ,
                spCost=ValueInfo{
                        currency= unCurrencySymbol adaSymbol,
                        token=  unTokenName adaToken,
                        value = 2000
                }
        }
data ValueInfo=ValueInfo{
    currency::ByteString,
    token:: ByteString,
    value:: Integer
} deriving(Generic,FromJSON,Prelude.Show,ToSchema)

valueInfoLovelace :: Integer -> ValueInfo
valueInfoLovelace=ValueInfo (unCurrencySymbol adaSymbol) (unTokenName adaToken)

valueInfoAssetClass:: ValueInfo -> AssetClass
valueInfoAssetClass (ValueInfo c t _)= AssetClass (CurrencySymbol c, TokenName t)

instance ToJSON ValueInfo
  where
    toJSON (ValueInfo c t v) = object [  "currency" .= (doConvert c), "token" .= (doConvert t),"value".=toJSON v]
      where

        doConvert bs= toJSON $ toText bs

        toText bs= encodeByteString bs

data NftsOnSaleResponse=NftsOnSaleResponse{
    cost::ValueInfo ,
    saleType:: SellType,
    fee:: Integer,
    owner:: ByteString,
    values:: [ValueInfo],
    reference :: TxOutRef
}deriving(Generic,FromJSON,ToJSON,Prelude.Show)

directSaleToResponse:: Market -> (TxOutRef, TxOutTx,DirectSale) -> NftsOnSaleResponse
directSaleToResponse market (txOutRef,txOutTx,DirectSale{dsCost,dsSeller,dsType}) =
        NftsOnSaleResponse{
            cost=priceToValueInfo dsCost ,
            saleType= dsType,
            fee= if dsType == Secondary  then mPrimarySaleFee  market else mSecondarySaleFee market,
            owner= getPubKeyHash dsSeller,
            values= toValueInfo (txOutValue (txOutTxOut  txOutTx)),
            reference=txOutRef
        }

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

priceToValueInfo::Price ->ValueInfo
priceToValueInfo (Price (c, t, v))=ValueInfo (unCurrencySymbol c) (convertString (unTokenName t)) v