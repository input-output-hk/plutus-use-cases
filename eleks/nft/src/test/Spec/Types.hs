{-# LANGUAGE OverloadedStrings  #-}

module Spec.Types
    where

import           Contracts.NFT          as NFTMarket
import qualified Spec.TestNFTCurrency   as NFTCurrency 
import           Ledger.Value           (CurrencySymbol(..), TokenName (..), AssetClass(..))
import qualified Data.ByteString.Char8  as B

nftMarketMock :: NFTMarket
nftMarketMock = NFTMarket{ marketId = createNFTTokenMock NFTMarket.marketplaceTokenName } 

data TestTokenMeta = TestTokenMeta
    { testTokenName:: TokenName
    , testTokenSymbol:: CurrencySymbol
    , testTokenClass:: AssetClass
    , testTokenDesciption:: String
    , testTokenAuthor:: String
    , testTokenFile:: String
    , testTokenMetaName :: TokenName
    , testTokenMetaSymbol :: CurrencySymbol
    , testTokenMetaClass :: AssetClass
    }

createTestToken:: TokenName -> TestTokenMeta
createTestToken tokenName = TestTokenMeta
    { testTokenName = tokenName
    , testTokenSymbol = getNFTTokenSymbol tokenName
    , testTokenClass = AssetClass (getNFTTokenSymbol tokenName, tokenName)
    , testTokenDesciption = "testTokenDescrition"
    , testTokenAuthor = "testTokenAuthor"
    , testTokenFile = "testTokenFile"
    , testTokenMetaName = tokenMetaName
    , testTokenMetaSymbol = getNFTTokenSymbol tokenMetaName
    , testTokenMetaClass = AssetClass (getNFTTokenSymbol tokenMetaName, tokenMetaName)
    } 
    where
        tokenMetaName = TokenName $ B.pack $ read (show tokenName) ++ "Metadata"

testToken1 = createTestToken "token1"
testToken1Meta = createNftMeta testToken1
testToken1MetaDto = nftMetadataToDto testToken1Meta
testToken2 = createTestToken "token2"
testToken2Meta = nftMetadataToDto $ createNftMeta testToken1
nonMarketToken1 = createTestToken "nonMarketToken1"

getNFTTokenSymbol:: TokenName -> CurrencySymbol
getNFTTokenSymbol tokenName = NFTCurrency.currencySymbol $ NFTCurrency.TestNFTCurrency tokenName

createNFTTokenMock:: TokenName -> AssetClass
createNFTTokenMock tokenName = AssetClass (getNFTTokenSymbol tokenName, tokenName)

nftMaketSellPrice:: Integer
nftMaketSellPrice = 1000

createNftMeta:: TestTokenMeta -> NFTMetadata
createNftMeta testToken = NFTMetadata
    { nftTokenName = testTokenName testToken
    , nftMetaTokenName = testTokenMetaName testToken
    , nftMetaDescription = B.pack $ testTokenDesciption testToken
    , nftMetaAuthor = B.pack $ testTokenAuthor testToken
    , nftMetaFile = B.pack $ testTokenFile testToken
    , nftTokenSymbol = testTokenSymbol testToken
    , nftMetaTokenSymbol = testTokenMetaSymbol testToken
    , nftSeller = Nothing
    , nftSellPrice = 0
    }