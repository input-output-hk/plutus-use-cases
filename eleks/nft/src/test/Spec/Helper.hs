{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NumericUnderscores   #-}

module Spec.Helper
    where

import           Contracts.NFT          as NFTMarket
import qualified Data.ByteString.Char8  as B
import qualified Data.Semigroup         as Semigroup
import           Data.String            (fromString)
import           Data.Monoid            (Last (..))
import           Data.Text              (Text)
import           Data.Void              (Void)
import qualified Spec.MockNFTCurrency   as MockCurrency
import           Ledger                 (PubKeyHash, pubKeyHash)
import           Ledger.Value           (CurrencySymbol(..), TokenName (..), AssetClass(..), toString)
import qualified Plutus.Trace.Emulator  as Trace
import           PlutusTx.Prelude       (toBuiltin)
import           Wallet.Emulator        (Wallet(..), walletPubKey)

ownerWallet' :: Wallet
ownerWallet' = Wallet 5

mockMarketId :: AssetClass
mockMarketId = createMarketTokenMock NFTMarket.marketplaceTokenName

mockNftCurrency :: NFTCurrency
mockNftCurrency = NFTMarket.mkNFTCurrency mockMarketId

nftMarketMock :: NFTMarket
nftMarketMock = NFTMarket
    { marketId = mockMarketId
    , marketTokenSymbol = nftCurrencySymbol mockNftCurrency
    , marketTokenMetaSymbol = nftCurrencySymbol mockNftCurrency
    , marketTokenMetaNameSuffix = toBuiltin . B.pack $  metadataTokenNamePrefix
    , marketFee = 500000
    , marketOwner = pubKeyHash $ walletPubKey ownerWallet'
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
        tokenMetaName = fromString $ (toString tokenName) ++ "Metadata"

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
    , nftMetaDescription = toBuiltin . B.pack . testTokenDesciption $ testToken
    , nftMetaAuthor = toBuiltin. B.pack . testTokenAuthor $ testToken
    , nftMetaFile = toBuiltin . B.pack . testTokenFile $ testToken
    , nftTokenSymbol = testTokenSymbol testToken
    , nftMetaTokenSymbol = testTokenMetaSymbol testToken
    , nftSeller = testTokenSeller testToken
    , nftSellPrice = testTokenSellPrice testToken
    }

extractNFTMarket:: Trace.ContractHandle ( Last (Either Text NFTMarket)) MarketOwnerSchema Void -> Trace.EmulatorTrace NFTMarket
extractNFTMarket handle = do
    t <- Trace.observableState handle
    
    case t of
        Last (Just (Right market)) -> return market
        _                          -> Trace.throwError (Trace.GenericError "market not found")

extractTokenMeta:: 
    Trace.ContractHandle ( Last (Either Text MarketContractState)) MarketUserSchema Void -> Trace.EmulatorTrace NFTMetadataDto
extractTokenMeta handle = do
    t <- Trace.observableState handle
    case t of
        Data.Monoid.Last (Just (Right (NFTMarket.Created nftMeta))) -> return nftMeta
        _                                                           -> 
            Trace.throwError (Trace.GenericError "created nft metadata not found")

extractCurrencyForgedNFT:: 
    Trace.ContractHandle (Maybe (Semigroup.Last MockCurrency.MockNFTCurrency)) MockCurrency.CurrencySchema Text
    -> Trace.EmulatorTrace MockCurrency.MockNFTCurrency
extractCurrencyForgedNFT handle = do
    t <- Trace.observableState handle
    case t of
        Just (Semigroup.Last currency) -> return currency
        _                              -> Trace.throwError (Trace.GenericError "currency not found")