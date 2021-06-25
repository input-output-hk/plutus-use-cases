{-# LANGUAGE OverloadedStrings  #-}

module Spec.Types
    where

import           Contracts.NFT          as NFTMarket
import qualified Spec.MockNFTCurrency   as MockCurrency
import           Ledger                 (PubKeyHash, pubKeyHash)
import           Ledger.Value           (CurrencySymbol(..), TokenName (..), AssetClass(..))
import qualified Data.ByteString.Char8  as B
import           Wallet.Emulator        (Wallet, walletPubKey)

mockMarketId :: AssetClass
mockMarketId = createMarketTokenMock NFTMarket.marketplaceTokenName

mockNftCurrency :: NFTCurrency
mockNftCurrency = NFTMarket.mkNFTCurrency mockMarketId

nftMarketMock :: NFTMarket
nftMarketMock = NFTMarket
    { marketId = mockMarketId
    , marketTokenSymbol = nftCurrencySymbol mockNftCurrency
    , marketTokenMetaSymbol = nftCurrencySymbol mockNftCurrency
    , marketTokenMetaNameSuffix = B.pack metadataTokenNamePrefix
    } 

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
    , testTokenSeller :: Maybe PubKeyHash
    , testTokenSellPrice :: Integer
    }

createTestToken:: TokenName -> TestTokenMeta
createTestToken tokenName = TestTokenMeta
    { testTokenName = tokenName
    , testTokenSymbol = marketTokenSymbol nftMarketMock
    , testTokenClass = AssetClass (NFTMarket.marketTokenSymbol nftMarketMock, tokenName)
    , testTokenDesciption = "testTokenDescrition"
    , testTokenAuthor = "testTokenAuthor"
    , testTokenFile = "testTokenFile"
    , testTokenMetaName = tokenMetaName
    , testTokenMetaSymbol = NFTMarket.marketTokenMetaSymbol nftMarketMock
    , testTokenMetaClass = AssetClass ( NFTMarket.marketTokenMetaSymbol nftMarketMock, tokenMetaName)
    , testTokenSeller = Nothing
    , testTokenSellPrice = 0
    } 
    where
        tokenMetaName = TokenName $ B.pack $ read (show tokenName) ++ "Metadata"

makeSellingTestToken:: TestTokenMeta -> Wallet -> Integer -> TestTokenMeta
makeSellingTestToken testToken wallet price =
    testToken{testTokenSellPrice = nftMaketSellPrice, testTokenSeller = Just $ pubKeyHash $ walletPubKey wallet }

testToken1 = createTestToken "token1"
testToken1Meta = createNftMeta testToken1
testToken1MetaDto = nftMetadataToDto testToken1Meta
testToken2 = createTestToken "token2"
testToken2Meta = createNftMeta testToken2
testToken2MetaDto = nftMetadataToDto testToken2Meta
testToken3 = createTestToken "token3"
nonMarketToken1 = createTestToken "nonMarketToken1"


getMarketTokenSymbol:: TokenName -> CurrencySymbol
getMarketTokenSymbol tokenName = MockCurrency.currencySymbol $ MockCurrency.MockNFTCurrency tokenName

createMarketTokenMock:: TokenName -> AssetClass
createMarketTokenMock tokenName = AssetClass (getMarketTokenSymbol tokenName, tokenName)

nftMaketSellPrice:: Integer
nftMaketSellPrice = 1000

toMetaDto:: TestTokenMeta ->  NFTMetadataDto
toMetaDto = nftMetadataToDto . createNftMeta

createNftMeta:: TestTokenMeta -> NFTMetadata
createNftMeta testToken = NFTMetadata
    { nftTokenName = testTokenName testToken
    , nftMetaTokenName = testTokenMetaName testToken
    , nftMetaDescription = B.pack $ testTokenDesciption testToken
    , nftMetaAuthor = B.pack $ testTokenAuthor testToken
    , nftMetaFile = B.pack $ testTokenFile testToken
    , nftTokenSymbol = testTokenSymbol testToken
    , nftMetaTokenSymbol = testTokenMetaSymbol testToken
    , nftSeller = testTokenSeller testToken
    , nftSellPrice = testTokenSellPrice testToken
    }