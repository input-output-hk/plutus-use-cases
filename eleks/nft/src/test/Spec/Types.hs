{-# LANGUAGE OverloadedStrings  #-}

module Spec.Types
    where

import           Contracts.NFT          as NFTMarket
import           Spec.TestNFTCurrency   as NFTCurrency 
import           Ledger.Value           (CurrencySymbol(..), TokenName (..), AssetClass(..))
import qualified Data.ByteString.Char8  as B

nftMarketMock :: NFTMarket
nftMarketMock = NFTMarket{ marketId = createNFTTokenMock NFTMarket.marketplaceTokenName } 

nftTestTokenName :: TokenName
nftTestTokenName = "testToken"

nftTestTokenDescription :: String
nftTestTokenDescription = "testTokenDescrition"

nftTestTokenAuthor :: String
nftTestTokenAuthor = "testTokenAuthor"

nftTestTokenFile :: String 
nftTestTokenFile = "testTokenFile"

nftTestToken :: AssetClass
nftTestToken = createNFTTokenMock nftTestTokenName

nftTestTokenCurrencySymbol :: CurrencySymbol
nftTestTokenCurrencySymbol = getNFTTokenSymbol nftTestTokenName

nftTestMeta :: NFTMetadata
nftTestMeta = createNftMeta nftTestTokenName nftTestTokenCurrencySymbol

nftTestTokenMeta:: NFTMetadataDto
nftTestTokenMeta = nftMetadataToDto nftTestMeta

nftNonMarketTokenName :: TokenName
nftNonMarketTokenName = "nonMarketNft"

nftNonMarketTestToken :: AssetClass
nftNonMarketTestToken = createNFTTokenMock nftNonMarketTokenName

nftTestTokenMetadataName :: TokenName
nftTestTokenMetadataName = "testTokenMetadata"

nftTestTokenMetadata :: AssetClass
nftTestTokenMetadata = createNFTTokenMock nftTestTokenMetadataName

getNFTTokenSymbol:: TokenName -> CurrencySymbol
getNFTTokenSymbol tokenName = NFTCurrency.currencySymbol $ TestNFTCurrency tokenName

createNFTTokenMock:: TokenName -> AssetClass
createNFTTokenMock tokenName = AssetClass (getNFTTokenSymbol tokenName, tokenName)

createNftMeta:: TokenName -> CurrencySymbol -> NFTMetadata
createNftMeta tokenName currency = NFTMetadata
    { nftTokenName = tokenName
    , nftMetaTokenName = tokenName
    , nftMetaDescription = B.pack nftTestTokenDescription
    , nftMetaAuthor = B.pack nftTestTokenAuthor
    , nftMetaFile = B.pack nftTestTokenFile
    , nftTokenSymbol = currency
    , nftMetaTokenSymbol = currency
    , nftSeller = Nothing
    , nftSellPrice = 0
    }