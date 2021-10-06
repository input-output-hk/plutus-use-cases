{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# options_ghc -fno-warn-orphans          #-}
{-# options_ghc -Wno-redundant-constraints #-}
{-# options_ghc -fno-strictness            #-}
{-# options_ghc -fno-specialise            #-}

module Contracts.NFT.Types
  where

import           Ledger
import           Ledger.Value        (AssetClass (..), assetClass, assetClassValue, assetClassValueOf)
import           Playground.Contract (Show, FromJSON, Generic, ToJSON, ToSchema)
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude
import           Prelude             (String)
import           Text.Printf         (PrintfArg)

-- | An nft market
data NFTMarket = NFTMarket
    { marketId :: AssetClass
    , marketTokenSymbol :: CurrencySymbol
    , marketTokenMetaSymbol :: CurrencySymbol
    , marketTokenMetaNameSuffix:: BuiltinByteString
    , marketFee :: Integer
    , marketOwner :: PubKeyHash
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''NFTMarket

data NFTMetadata = NFTMetadata
    { 
      nftTokenName:: TokenName
    , nftMetaTokenName:: TokenName
    , nftMetaDescription:: BuiltinByteString
    , nftMetaAuthor:: BuiltinByteString
    , nftMetaFile:: BuiltinByteString
    , nftTokenSymbol :: CurrencySymbol
    , nftMetaTokenSymbol :: CurrencySymbol
    , nftSeller :: Maybe PubKeyHash
    , nftSellPrice:: Integer
    }
    deriving (Show, Generic, ToJSON, FromJSON)

instance Eq NFTMetadata where
    {-# INLINABLE (==) #-}
    x == y = (nftTokenSymbol x PlutusTx.Prelude.== nftTokenSymbol y) && 
             (nftMetaTokenSymbol x PlutusTx.Prelude.== nftMetaTokenSymbol y) &&
             (nftTokenName x PlutusTx.Prelude.== nftTokenName y) &&
             (nftMetaTokenName x PlutusTx.Prelude.== nftMetaTokenName y)

PlutusTx.makeIsDataIndexed ''NFTMetadata [('NFTMetadata, 0)]
PlutusTx.makeLift ''NFTMetadata
    
data NFTMarketAction = Create NFTMetadata | Sell | CancelSell | Buy PubKeyHash
    deriving Show

PlutusTx.makeIsDataIndexed ''NFTMarketAction [ ('Create , 0)
                                           , ('Sell,   1)
                                           , ('CancelSell,   2)
                                           , ('Buy,   3)
                                           ]
PlutusTx.makeLift ''NFTMarketAction

data NFTMarketDatum =
      Factory [NFTMetadata]
    | NFTMeta NFTMetadata
    deriving stock (Show)

PlutusTx.unstableMakeIsData ''NFTMarketDatum
PlutusTx.makeLift ''NFTMarketDatum

-- | Data transfer object, used to send repsonse to the frontend,
-- | it allows to avoid using of cabal repl for create complex request objects 
data NFTMetadataDto = NFTMetadataDto
    { nftDtoTokenName:: String
    , nftDtoMetaDescription:: String
    , nftDtoMetaAuthor:: String
    , nftDtoMetaFile:: String
    , nftDtoTokenSymbol :: String
    , nftDtoSeller :: String
    , nftDtoSellPrice:: Integer
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance Eq NFTMetadataDto where
  x == y = (nftDtoTokenName x Prelude.== nftDtoTokenName y)
        && (nftDtoMetaDescription x Prelude.== nftDtoMetaDescription y)
        && (nftDtoMetaAuthor x Prelude.== nftDtoMetaAuthor y)
        && (nftDtoMetaFile x Prelude.== nftDtoMetaFile y)
        && (nftDtoTokenSymbol x Prelude.== nftDtoTokenSymbol y)
        && (nftDtoSeller x Prelude.== nftDtoSeller y)
        && (nftDtoSellPrice x Prelude.== nftDtoSellPrice y)

{-# INLINABLE isNftToken #-}
isNftToken :: Value -> CurrencySymbol -> TokenName -> Bool
isNftToken v cur tokenName = assetClassValueOf v (assetClass cur tokenName) == 1

{-# INLINABLE getNftValue #-}
getNftValue :: CurrencySymbol -> TokenName -> Value
getNftValue cur tokenName = assetClassValue (assetClass cur tokenName) 1

{-# INLINABLE isMarketToken #-}
isMarketToken :: Value -> AssetClass -> Bool
isMarketToken v ac = assetClassValueOf v ac == 1

{-# INLINABLE valueWithin #-}
valueWithin :: TxInInfo -> Value
valueWithin = txOutValue . txInInfoResolved