{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}

module Spec.NFT
    ( tests
    ) where

import           Control.Monad          (void)
import           Ledger.Ada             (adaValueOf)
import           Plutus.Contract        hiding (when)
import           Plutus.Contract        (Contract, ContractError, HasBlockchainActions, BlockchainActions)
import           Plutus.Contract.Test
import           Contracts.NFT          as NFTMarket
import qualified Data.ByteString.Char8  as B
import           Plutus.Trace.Emulator  (ContractInstanceTag, EmulatorTrace)
import qualified Plutus.Trace.Emulator  as Trace
import qualified PlutusTx
import qualified PlutusTx.Prelude       as PlutusTx
import           Test.Tasty
import qualified Test.Tasty.HUnit       as HUnit
import           Data.Monoid            (Last (..))
import           Ledger.Value           (CurrencySymbol(..), TokenName (..), Value, AssetClass(..), assetClassValue)
import           Ledger                 (pubKeyAddress, PubKeyHash)
import           Ledger.Ada                 as Ada
import           Data.Text              (Text, pack)
import           Data.Void              (Void)
import           Data.Aeson             (FromJSON (..), Result (..), ToJSON (..), genericToJSON, genericParseJSON
                                        , defaultOptions, Options(..), decode, encode, parseJSON, fromJSON)
import           Control.Monad.Freer.Extras as Extras
import           Spec.TestNFTCurrency   as NFTCurrency 
w1, w2 :: Wallet
w1 = Wallet 1
w2 = Wallet 2

t1, t2 :: ContractInstanceTag
t1 = Trace.walletInstanceTag w1
t2 = Trace.walletInstanceTag w2

ownerContract :: Contract (Last (Either Text NFTMarket)) MarketOwnerSchema Void ()
ownerContract = NFTMarket.ownerEndpoint forgeMockNftToken
userContract :: Contract (Last (Either Text MarketContractState)) MarketUserSchema Void ()
userContract = NFTMarket.userEndpoints forgeMockNftToken nftMarketMock

tests :: TestTree
tests = testGroup "nft"
    [
        checkPredicate "Owner contract expose 'start' endpoints"
        (
            endpointAvailable @"start" ownerContract t1
        )
        activeOwnerContractTrace
        ,
        checkPredicate "Should create NFT token"
        ( 
           assertNoFailedTransactions
           .&&. valueAtAddress (marketAddress nftMarketMock) 
                (== (assetClassValue nftTokenMetadata 1 
                    <> assetClassValue (marketId nftMarketMock) 1)
                )
           .&&. walletFundsChange (Wallet 1) (assetClassValue nftToken 1)
        )
        createNftTokenFlowTrace
        ,
        checkPredicate "Should sell NFT token"
        ( 
           assertNoFailedTransactions
           .&&. valueAtAddress (marketAddress nftMarketMock) 
                (== (assetClassValue nftToken 1 
                    <> assetClassValue nftTokenMetadata 1 
                    <> assetClassValue (marketId nftMarketMock) 1))
            -- create and send token in one trace
           .&&. walletFundsChange (Wallet 1) (Ada.lovelaceValueOf 0)
        )
        sellNftTokenFlowTrace
        ,
        checkPredicate "Should buy NFT token"
        ( 
           assertNoFailedTransactions
           .&&. valueAtAddress (marketAddress nftMarketMock) (== (assetClassValue nftTokenMetadata 1 <> assetClassValue (marketId nftMarketMock) 1))
           .&&. walletFundsChange (Wallet 1) (Ada.lovelaceValueOf 1000)
           .&&. walletFundsChange (Wallet 2) (Ada.lovelaceValueOf (-1000) <> assetClassValue nftToken 1)
        )
        buyNftTokenFlowTrace
    ]

initialise :: EmulatorTrace ()
initialise = do
    ownerHdl <- Trace.activateContractWallet w1 ownerContract
    Trace.callEndpoint @"start" ownerHdl ()
    void $ Trace.waitNSlots 5

createNftTokenFlowTrace :: EmulatorTrace ()
createNftTokenFlowTrace = do
    initialise
    void $ createNftTokenTrace w1

sellNftTokenFlowTrace :: EmulatorTrace ()
sellNftTokenFlowTrace = do
    initialise
    nftTokenMeta <- createNftTokenTrace w1
    sellNftTokenTrace w1 nftTokenMeta

buyNftTokenFlowTrace :: EmulatorTrace ()
buyNftTokenFlowTrace = do
    initialise
    nftTokenMeta <- createNftTokenTrace w1
    sellNftTokenTrace w1 nftTokenMeta
    buyNftTokenTrace w2 nftTokenMeta

forgeMockNftToken:: 
    forall w s. HasBlockchainActions s 
    => TokenName
    -> PubKeyHash
    -> Contract w s Text CurrencySymbol
forgeMockNftToken tokenName pk = 
    NFTCurrency.currencySymbol 
    <$> NFTCurrency.forgeContract pk tokenName

nftMarketMock :: NFTMarket
nftMarketMock = NFTMarket{ marketId = createNFTTokenMock NFTMarket.marketplaceTokenName } 

nftTokenName :: TokenName
nftTokenName = "testToken"
nftToken :: AssetClass
nftToken = createNFTTokenMock nftTokenName

nftTokenMetadataName :: TokenName
nftTokenMetadataName = "testTokenMetadata"
nftTokenMetadata :: AssetClass
nftTokenMetadata = createNFTTokenMock nftTokenMetadataName

createNFTTokenMock:: TokenName -> AssetClass
createNFTTokenMock tokenName = AssetClass (NFTCurrency.currencySymbol $ TestNFTCurrency tokenName, tokenName)

activeOwnerContractTrace :: EmulatorTrace ()
activeOwnerContractTrace = void $ Trace.activateContractWallet w1 ownerContract

activeUserContractTrace :: EmulatorTrace ()
activeUserContractTrace = void $ Trace.activateContractWallet w1 userContract

createNftTokenTrace :: Wallet -> EmulatorTrace NFTMetadataDto
createNftTokenTrace w = do
    userMarketHdl <- Trace.activateContractWallet w userContract
    let nftTokenParams = NFTMarket.CreateParams { cpTokenName = "testToken", cpDescription = "TestDescrition", cpAuthor = "Author1", cpFile = "file1" }
    Trace.callEndpoint @"create" userMarketHdl nftTokenParams
    void $ Trace.waitNSlots 5
    extractTokenMeta userMarketHdl

sellNftTokenTrace :: Wallet -> NFTMetadataDto -> EmulatorTrace ()
sellNftTokenTrace w nftTokenMeta = do
    userMarketHdl <- Trace.activateContractWallet w userContract
    let nftTokenSellParams = NFTMarket.SellParams { spTokenSymbol = nftDtoTokenSymbol nftTokenMeta, spSellPrice = 1000}
    Trace.callEndpoint @"sell" userMarketHdl nftTokenSellParams
    void $ Trace.waitNSlots 5

buyNftTokenTrace :: Wallet -> NFTMetadataDto -> EmulatorTrace ()
buyNftTokenTrace w nftTokenMeta = do
    userMarketHdl <- Trace.activateContractWallet w userContract
    let nftTokenBuyParams = NFTMarket.BuyParams { bpTokenSymbol = nftDtoTokenSymbol nftTokenMeta }
    Trace.callEndpoint @"buy" userMarketHdl nftTokenBuyParams
    void $ Trace.waitNSlots 5

extractNFTMarket:: Trace.ContractHandle (Last (Either Text NFTMarket)) MarketOwnerSchema Void -> Trace.EmulatorTrace NFTMarket
extractNFTMarket handle = do
    t <- Trace.observableState handle
    
    case t of
        Last (Just (Right market)) -> pure market
        _                          -> Trace.throwError (Trace.GenericError "market not found")

extractTokenMeta:: Trace.ContractHandle (Last (Either Text MarketContractState)) MarketUserSchema Void -> Trace.EmulatorTrace NFTMetadataDto
extractTokenMeta handle = do
    t <- Trace.observableState handle
    case t of
        Last (Just (Right (NFTMarket.Created nftMeta))) -> pure nftMeta
        _                                               -> Trace.throwError (Trace.GenericError "created nft metadata not found")