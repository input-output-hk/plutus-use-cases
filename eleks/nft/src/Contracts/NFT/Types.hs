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

-- Note: An orphan instance here because of the alias above.
--deriving anyclass instance ToSchema AssetClass

data NFTMetadata = NFTMetadata
    { 
      nftTokenName:: TokenName
    , nftMetaTokenName:: TokenName
    , nftMetaDescription:: ByteString
    , nftMetaAuthor:: ByteString
    , nftMetaFile:: ByteString
    , nftTokenSymbol :: CurrencySymbol
    , nftMetaTokenSymbol :: CurrencySymbol
    , nftSeller :: Maybe PubKeyHash
    , nftSellPrice:: Integer
    }
    deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance Eq NFTMetadata where
    {-# INLINABLE (==) #-}
    x == y = (nftTokenSymbol x PlutusTx.Prelude.== nftTokenSymbol y) && 
             (nftMetaTokenSymbol x PlutusTx.Prelude.== nftMetaTokenSymbol y)

PlutusTx.makeIsDataIndexed ''NFTMetadata [('NFTMetadata, 0)]
PlutusTx.makeLift ''NFTMetadata

newtype NFTMarket = NFTMarket
    { marketId :: AssetClass
    } deriving stock    (Show, Generic)
      deriving anyclass (ToJSON, FromJSON, ToSchema)
      deriving newtype  (Prelude.Eq, Prelude.Ord)


PlutusTx.makeLift ''NFTMarket
    
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


        