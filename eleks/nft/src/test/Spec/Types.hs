{-# LANGUAGE OverloadedStrings  #-}

module Spec.Types
    where

import           Contracts.NFT          as NFTMarket
import qualified Spec.TestNFTCurrency   as NFTCurrency 
import           Ledger                 (PubKeyHash, pubKeyHash)
import           Ledger.Value           (CurrencySymbol(..), TokenName (..), AssetClass(..))
import qualified Data.ByteString.Char8  as B
import           Wallet.Emulator        (Wallet, walletPubKey)

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
    , testTokenSeller :: Maybe PubKeyHash
    , testTokenSellPrice :: Integer
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

getNFTTokenSymbol:: TokenName -> CurrencySymbol
getNFTTokenSymbol tokenName = NFTCurrency.currencySymbol $ NFTCurrency.TestNFTCurrency tokenName

createNFTTokenMock:: TokenName -> AssetClass
createNFTTokenMock tokenName = AssetClass (getNFTTokenSymbol tokenName, tokenName)

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