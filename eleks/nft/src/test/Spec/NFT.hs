{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE LambdaCase         #-}

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
import           Ledger                 (pubKeyAddress, PubKeyHash)
import           Ledger.Ada             as Ada
import           Ledger.Index           (ValidationError (ScriptFailure))
import           Ledger.Scripts         (ScriptError (EvaluationError))
import           Ledger.Value           (CurrencySymbol(..), TokenName (..), Value, AssetClass(..), assetClassValue)
import           Data.Text              (Text, pack)
import           Data.Void              (Void)
import           Data.Aeson             (FromJSON (..), Result (..), ToJSON (..), genericToJSON, genericParseJSON
                                        , defaultOptions, Options(..), decode, encode, parseJSON, fromJSON)
import           Control.Monad.Freer.Extras as Extras
import           Spec.TestNFTCurrency   as NFTCurrency 
import qualified Data.Semigroup         as Semigroup

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
                (== (assetClassValue nftTestTokenMetadata 1 
                    <> assetClassValue (marketId nftMarketMock) 1)
                )
           .&&. walletFundsChange (Wallet 1) (assetClassValue nftTestToken 1)
        )
        createNftTokenFlowTrace
        ,
        checkPredicate "Should fail if duplicate"
        ( 
            assertFailedTransaction (\_ err _ -> case err of {ScriptFailure (EvaluationError ["nft token is arleady exists"]) -> True; _ -> False  })
        )
        createDuplicateNftTokenFailureTrace
        ,
        checkPredicate "Should sell NFT token"
        ( 
           assertNoFailedTransactions
           .&&. valueAtAddress (marketAddress nftMarketMock) 
                (== (assetClassValue nftTestToken 1 
                    <> assetClassValue nftTestTokenMetadata 1 
                    <> assetClassValue (marketId nftMarketMock) 1))
            -- create and send token in one trace
           .&&. walletFundsChange (Wallet 1) (Ada.lovelaceValueOf 0)
        )
        sellNftTokenFlowTrace
        --,
        -- checkPredicate "Should fail if non market token"
        -- ( 
        --     assertContractError userContract t1 (\case { _ -> True; _ -> True}) "error should match"
        -- )
        -- sellNonMarketNFTFailureTrace
        ,
        checkPredicate "Should buy NFT token"
        ( 
           assertNoFailedTransactions
           .&&. valueAtAddress (marketAddress nftMarketMock) (== (assetClassValue nftTestTokenMetadata 1 <> assetClassValue (marketId nftMarketMock) 1))
           .&&. walletFundsChange w1 (Ada.lovelaceValueOf 1000)
           .&&. walletFundsChange w2 (Ada.lovelaceValueOf (-1000) <> assetClassValue nftTestToken 1)
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
    void $ createNftTokenTrace w1 nftTestTokenName

createDuplicateNftTokenFailureTrace :: EmulatorTrace ()
createDuplicateNftTokenFailureTrace = do
    initialise
    void $ createNftTokenTrace w1 nftTestTokenName
    void $ createNftTokenTrace w1 nftTestTokenName

sellNftTokenFlowTrace :: EmulatorTrace ()
sellNftTokenFlowTrace = do
    initialise
    nftTokenMeta <- createNftTokenTrace w1 nftTestTokenName
    sellNftTokenTrace w1 nftTokenMeta

sellNonMarketNFTFailureTrace  :: EmulatorTrace ()
sellNonMarketNFTFailureTrace = do
    initialise

    nonMarketNftMeta <- createNonMarketNftTokenTrace w1 nftNonMarketTokenName

    Extras.logInfo @String "before sale"
    sellNftTokenTrace w1 nonMarketNftMeta

buyNftTokenFlowTrace :: EmulatorTrace ()
buyNftTokenFlowTrace = do
    initialise
    nftTokenMeta <- createNftTokenTrace w1 nftTestTokenName
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

nftTestTokenName :: TokenName
nftTestTokenName = "testToken"
nftTestToken :: AssetClass
nftTestToken = createNFTTokenMock nftTestTokenName

nftNonMarketTokenName :: TokenName
nftNonMarketTokenName = "nonMarketNft"
nftNonMarketTestToken :: AssetClass
nftNonMarketTestToken = createNFTTokenMock nftNonMarketTokenName

nftTestTokenMetadataName :: TokenName
nftTestTokenMetadataName = "testTokenMetadata"
nftTestTokenMetadata :: AssetClass
nftTestTokenMetadata = createNFTTokenMock nftTestTokenMetadataName

createNFTTokenMock:: TokenName -> AssetClass
createNFTTokenMock tokenName = AssetClass (NFTCurrency.currencySymbol $ TestNFTCurrency tokenName, tokenName)

activeOwnerContractTrace :: EmulatorTrace ()
activeOwnerContractTrace = void $ Trace.activateContractWallet w1 ownerContract

activeUserContractTrace :: EmulatorTrace ()
activeUserContractTrace = void $ Trace.activateContractWallet w1 userContract

createNftTokenTrace :: Wallet -> TokenName ->  EmulatorTrace NFTMetadataDto
createNftTokenTrace w tokenName = do
    userMarketHdl <- Trace.activateContractWallet w userContract
    let nftTokenParams = NFTMarket.CreateParams { cpTokenName = read . show $ tokenName, cpDescription = "TestDescrition", cpAuthor = "Author1", cpFile = "file1" }
    Trace.callEndpoint @"create" userMarketHdl nftTokenParams
    void $ Trace.waitNSlots 5
    extractTokenMeta userMarketHdl

createNonMarketNftTokenTrace :: Wallet -> TokenName ->  EmulatorTrace NFTMetadataDto
createNonMarketNftTokenTrace w tokenName = do
    forgeCurrencyHdl <- Trace.activateContractWallet w1 NFTCurrency.forgeNftToken
    let nftTokenForgeParams = NFTCurrency.ForgeNftParams { fnpTokenName = tokenName }
    Trace.callEndpoint @"create" forgeCurrencyHdl nftTokenForgeParams
    void $ Trace.waitNSlots 5
    testNftCur <- extractCurrencyForgedNFT forgeCurrencyHdl
    pure $ nftMetadataToDto $ NFTMetadata
        { nftTokenName = tokenName
        , nftMetaDescription = "NonDto"
        , nftMetaAuthor ="Author"
        , nftMetaFile = "File"
        , nftTokenSymbol = currencySymbol testNftCur
        }

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

extractNFTMarket:: Trace.ContractHandle ( Last (Either Text NFTMarket)) MarketOwnerSchema Void -> Trace.EmulatorTrace NFTMarket
extractNFTMarket handle = do
    t <- Trace.observableState handle
    
    case t of
        Last (Just (Right market)) -> pure market
        _                          -> Trace.throwError (Trace.GenericError "market not found")

extractTokenMeta:: Trace.ContractHandle ( Last (Either Text MarketContractState)) MarketUserSchema Void -> Trace.EmulatorTrace NFTMetadataDto
extractTokenMeta handle = do
    Extras.logInfo @String "urax6"
    t <- Trace.observableState handle

    Extras.logInfo @String "urax7"
    case t of
        Data.Monoid.Last (Just (Right (NFTMarket.Created nftMeta))) -> do
            Extras.logInfo @String "urax3"
            pure nftMeta
        _                                               -> do
            Extras.logInfo @String "urax4" 
            Trace.throwError (Trace.GenericError "created nft metadata not found")

extractCurrencyForgedNFT:: Trace.ContractHandle (Maybe (Semigroup.Last TestNFTCurrency)) NFTCurrency.CurrencySchema Text -> Trace.EmulatorTrace TestNFTCurrency
extractCurrencyForgedNFT handle = do
    t <- Trace.observableState handle
    case t of
        Just (Semigroup.Last currency) -> pure currency
        _                              -> Trace.throwError (Trace.GenericError "currency not found")