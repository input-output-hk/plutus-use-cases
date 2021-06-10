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

import           Control.Monad                          (void)
import           Ledger.Ada                             (adaValueOf)
import           Plutus.Contract                        hiding (when)
import           Plutus.Contract                        (Contract, ContractError, HasBlockchainActions, BlockchainActions)
import           Plutus.Contract.Test
import           Contracts.NFT                          as NFTMarket
import qualified Data.ByteString.Char8                   as B
import           Plutus.Trace.Emulator                  (ContractInstanceTag, EmulatorTrace)
import qualified Plutus.Trace.Emulator                  as Trace
import qualified PlutusTx
import           PlutusTx.Prelude                       as PlutusTx
import           Prelude                                (String, Char, read, show)
import           Test.Tasty
import qualified Test.Tasty.HUnit                       as HUnit
import           Data.Monoid                            (Last (..))
import           Data.Maybe                             (listToMaybe, mapMaybe)
import           Ledger                                 (pubKeyAddress, PubKeyHash, pubKeyHash)
import           Ledger.Ada                             as Ada
import           Ledger.Index                              (ValidationError (ScriptFailure))
import           Ledger.Scripts                         (ScriptError (EvaluationError))
import           Ledger.Value                           (CurrencySymbol(..), TokenName (..), Value, AssetClass(..), assetClassValue)
import           Data.Text                              (Text, pack)
import           Data.Void                              (Void)
import           Data.Aeson                             (FromJSON (..), Result (..), ToJSON (..), genericToJSON, genericParseJSON
                                                        , defaultOptions, Options(..), decode, encode, parseJSON, fromJSON)
import           Control.Monad.Freer.Extras             as Extras
import           Spec.TestNFTCurrency                   as NFTCurrency 
import           Spec.Types
import qualified Data.Semigroup                         as Semigroup
import qualified Wallet.Emulator                        as EM
import           Plutus.Trace.Emulator.Types            (_ContractLog, cilMessage, UserThreadMsg (..))
import qualified Wallet.Emulator.Folds                  as Folds
import qualified Control.Monad.Freer.Extras.Log         as Log
import           Wallet.Emulator.MultiAgent             (eteEvent)
import           Control.Lens                           (preview)
import qualified Plutus.Contracts.Currency              as PlutusCurrency
w1, w2 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
ownerWallet = Wallet 5

t1, t2 :: ContractInstanceTag
t1 = Trace.walletInstanceTag w1
t2 = Trace.walletInstanceTag w2
ownerInstanceTag = Trace.walletInstanceTag ownerWallet

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
            .&&. assertAccumState userContract t1 
                (\case Last (Just (Right (NFTMarket.Created meta))) -> meta == nftTestTokenMeta; _ -> False) 
                "should create NFT state"
        )
        createNftTokenFlowTrace
        ,
        -- checkPredicate "Should fail if duplicate"
        -- ( 
        --     assertFailedTransaction (\_ err _ -> case err of {ScriptFailure (EvaluationError ["nft token is arleady exists"]) -> True; _ -> False  })
        -- )
        -- createDuplicateNftTokenFailureTrace
        -- ,
        checkPredicate "Should sell NFT token"
        ( 
            assertNoFailedTransactions
            .&&. valueAtAddress (marketAddress nftMarketMock)
                (== (assetClassValue nftTestToken 1 
                    <> assetClassValue nftTestTokenMetadata 1 
                    <> assetClassValue (marketId nftMarketMock) 1))
            -- create and send token in one trace
            .&&. walletFundsChange (Wallet 1) (Ada.lovelaceValueOf 0)
            -- .&&. assertAccumState userContract t1
            -- (\case Last (Just (Right (NFTMarket.Selling meta))) -> 
            --             meta == (nftMetadataToDto $ nftTestMeta
            --             {nftSellPrice = 1000, nftSeller = Just $ pubKeyHash $ walletPubKey w1 });
            --         _ -> False) 
            --"should create sell NFT state"
        )
        sellNftTokenFlowTrace
        ,
        checkPredicate "Should buy NFT token"
        ( 
           assertNoFailedTransactions
           .&&. valueAtAddress (marketAddress nftMarketMock) (== (assetClassValue nftTestTokenMetadata 1 <> assetClassValue (marketId nftMarketMock) 1))
           .&&. walletFundsChange w1 (Ada.lovelaceValueOf 1000)
           .&&. walletFundsChange w2 (Ada.lovelaceValueOf (-1000) <> assetClassValue nftTestToken 1)
           .&&. assertAccumState userContract t2
                (\case Last (Just (Right (NFTMarket.Buyed meta))) -> meta == nftTestTokenMeta; _ -> False) 
                "should create buy NFT state"
        )
        buyNftTokenFlowTrace
    ]

initialise :: EmulatorTrace ()
initialise = do
    -- Used separate wallet for onwer contract because of error
    -- Failed to decode a 'Response JSON.Value'. The event is probably for a different 'Contract'. 
    -- This is often caused by having multiple contract instances share the same 'ContractInstanceTag' 
    -- (for example, when  using 'activateContractWallet' repeatedly on the same wallet). 
    -- To fix this, use 'activateContract' with a unique 'ContractInstanceTag' per instance.
    ownerHdl <- Trace.activateContractWallet ownerWallet ownerContract
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

-- testTrace  :: EmulatorTrace ()
-- testTrace = do
--     curHdl <- Trace.activateContractWallet  w1 (void PlutusCurrency.forgeCurrency)
--     Log.logInfo @String "Received contract state"
--     Trace.callEndpoint @"Create native token" curHdl PlutusCurrency.SimpleMPS { PlutusCurrency.tokenName="test", PlutusCurrency.amount = 1}
--     void $ Trace.waitNSlots 5

sellNonMarketNFTFailureTrace  :: EmulatorTrace ()
sellNonMarketNFTFailureTrace = do
    initialise
    nonMarketNftMeta <- createNonMarketNftTokenTrace w1 nftNonMarketTokenName
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

activeOwnerContractTrace :: EmulatorTrace ()
activeOwnerContractTrace = void $ Trace.activateContractWallet w1 ownerContract

activeUserContractTrace :: EmulatorTrace ()
activeUserContractTrace = void $ Trace.activateContractWallet w1 userContract

createNftTokenTrace :: Wallet -> TokenName ->  EmulatorTrace NFTMetadataDto
createNftTokenTrace w tokenName = do
    userMarketHdl <- Trace.activateContractWallet w userContract
    let nftTokenParams = NFTMarket.CreateParams { 
        cpTokenName = read . show $ tokenName
        , cpDescription = nftTestTokenDescription
        , cpAuthor = nftTestTokenAuthor
        , cpFile = nftTestTokenFile }
    Trace.callEndpoint @"create" userMarketHdl nftTokenParams
    void $ Trace.waitNSlots 5
    Extras.logInfo @String "after create"
    extractTokenMeta userMarketHdl

createNonMarketNftTokenTrace :: Wallet -> TokenName ->  EmulatorTrace NFTMetadataDto
createNonMarketNftTokenTrace w tokenName = do
    forgeCurrencyHdl <- Trace.activateContractWallet w1 NFTCurrency.forgeNftToken
    let nftTokenForgeParams = NFTCurrency.ForgeNftParams { fnpTokenName = tokenName }
    Trace.callEndpoint @"create" forgeCurrencyHdl nftTokenForgeParams
    void $ Trace.waitNSlots 5
    testNftCur <- extractCurrencyForgedNFT forgeCurrencyHdl
    return $ nftMetadataToDto $ createNftMeta tokenName $ currencySymbol testNftCur

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
        Last (Just (Right market)) -> return market
        _                          -> Trace.throwError (Trace.GenericError "market not found")

extractTokenMeta:: Trace.ContractHandle ( Last (Either Text MarketContractState)) MarketUserSchema Void -> Trace.EmulatorTrace NFTMetadataDto
extractTokenMeta handle = do
    t <- Trace.observableState handle
    case t of
        Data.Monoid.Last (Just (Right (NFTMarket.Created nftMeta))) -> do
            return nftMeta
        _                                               -> do
            Trace.throwError (Trace.GenericError "created nft metadata not found")

extractCurrencyForgedNFT:: Trace.ContractHandle (Maybe (Semigroup.Last TestNFTCurrency)) NFTCurrency.CurrencySchema Text -> Trace.EmulatorTrace TestNFTCurrency
extractCurrencyForgedNFT handle = do
    t <- Trace.observableState handle
    case t of
        Just (Semigroup.Last currency) -> return currency
        _                              -> Trace.throwError (Trace.GenericError "currency not found")