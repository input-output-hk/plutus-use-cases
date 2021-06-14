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
import qualified Prelude
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
import qualified Ledger.Value                     as Value

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
            .&&. walletFundsChange w1 (assetClassValue nftTestToken 1)
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
            .&&. walletFundsChange w1 (Ada.lovelaceValueOf 0)
            .&&. assertAccumState userContract t1
            (\case Last (Just (Right (NFTMarket.Selling meta))) -> 
                        meta == (nftMetadataToDto $ nftTestMeta
                        {nftSellPrice = 1000, nftSeller = Just $ pubKeyHash $ walletPubKey w1 });
                    _ -> False) 
                "should create sell NFT state"
        )
        sellNftTokenFlowTrace
        ,
        checkPredicate "Should fail start selling if non market nft"
        ( 
            assertNoFailedTransactions
            .&&. assertAccumState userContract t1
            (\case Last (Just (Left errText)) -> 
                    errText Prelude.== pack "Nft token not found";
                    _ -> False) 
                "should have failed state"
        )
        sellNonMarketNFTFailureTrace
        ,
        checkPredicate "Should cancel NFT token selling"
        ( 
            assertNoFailedTransactions
            .&&. valueAtAddress (marketAddress nftMarketMock)
                (== (assetClassValue nftTestTokenMetadata 1 
                    <> assetClassValue (marketId nftMarketMock) 1))
            -- create and send token in one trace
            .&&. walletFundsChange w1 (assetClassValue nftTestToken 1)
            .&&. assertAccumState userContract t1
            (\case Last (Just (Right (NFTMarket.CancelSelling meta))) -> 
                         meta == nftTestTokenMeta;
                    _ -> False) 
                "should cancel selling NFT state"
        )
        cancelSellNftTokenFlowTrace
        ,
        checkPredicate "Should fail cancel sell if not token not on sale"
        ( 
            assertNoFailedTransactions
            .&&. assertAccumState userContract t1
            (\case Last (Just (Left errText)) -> 
                    errText Prelude.== pack "NFT token is not on sale";
                    _ -> False) 
                "should have failed state "
        )
        cancelSellFailureIfNotSale
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
    -- need to have separate wallet for start contract
    -- https://github.com/input-output-hk/plutus/issues/3359
    ownerHdl <- Trace.activateContractWallet ownerWallet ownerContract
    Trace.callEndpoint @"start" ownerHdl ()
    void $ Trace.waitNSlots 5

createNftTokenFlowTrace :: EmulatorTrace ()
createNftTokenFlowTrace = do
    initialise
    user1Hdl <- Trace.activateContractWallet w1 userContract
    void $ createNftTokenTrace user1Hdl nftTestTokenName

createDuplicateNftTokenFailureTrace :: EmulatorTrace ()
createDuplicateNftTokenFailureTrace = do
    initialise
    user1Hdl <- Trace.activateContractWallet w1 userContract
    void $ createNftTokenTrace user1Hdl nftTestTokenName
    void $ createNftTokenTrace user1Hdl nftTestTokenName

sellNftTokenFlowTrace :: EmulatorTrace ()
sellNftTokenFlowTrace = do
    initialise
    user1Hdl <- Trace.activateContractWallet w1 userContract
    nftTokenMeta <- createNftTokenTrace user1Hdl nftTestTokenName
    sellNftTokenTrace user1Hdl nftTokenMeta

sellNonMarketNFTFailureTrace  :: EmulatorTrace ()
sellNonMarketNFTFailureTrace = do
    initialise
    forgeCurrencyHdl <- Trace.activateContractWallet w2 NFTCurrency.forgeNftToken
    (nonMarketNftMeta, nonNftCur) <- createNonMarketNftTokenTrace forgeCurrencyHdl nftNonMarketTokenName
    -- need to have separate wallet for Currency contract
    -- https://github.com/input-output-hk/plutus/issues/3359
    _ <- Trace.payToWallet w2 w1 $ Value.singleton (currencySymbol nonNftCur) nftNonMarketTokenName 1
    user1Hdl <- Trace.activateContractWallet w1 userContract
    sellNftTokenTrace user1Hdl nonMarketNftMeta

cancelSellNftTokenFlowTrace :: EmulatorTrace ()
cancelSellNftTokenFlowTrace = do
    initialise
    user1Hdl <- Trace.activateContractWallet w1 userContract
    nftTokenMeta <- createNftTokenTrace user1Hdl nftTestTokenName
    sellNftTokenTrace user1Hdl nftTokenMeta
    cancelSellNftTokenTrace user1Hdl nftTokenMeta

cancelSellFailureIfNotSale :: EmulatorTrace ()
cancelSellFailureIfNotSale= do
    initialise
    user1Hdl <- Trace.activateContractWallet w1 userContract
    nftTokenMeta <- createNftTokenTrace user1Hdl nftTestTokenName
    cancelSellNftTokenTrace user1Hdl nftTokenMeta

buyNftTokenFlowTrace :: EmulatorTrace ()
buyNftTokenFlowTrace = do
    initialise
    user1Hdl <- Trace.activateContractWallet w1 userContract
    user2Hdl <- Trace.activateContractWallet w2 userContract
    nftTokenMeta <- createNftTokenTrace user1Hdl nftTestTokenName
    sellNftTokenTrace user1Hdl nftTokenMeta
    buyNftTokenTrace user2Hdl nftTokenMeta

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

createNftTokenTrace :: 
    Trace.ContractHandle (Last(Either Text MarketContractState)) MarketUserSchema Void 
    -> TokenName 
    -> EmulatorTrace NFTMetadataDto
createNftTokenTrace hdl tokenName = do
    let nftTokenParams = NFTMarket.CreateParams { 
        cpTokenName = read . show $ tokenName
        , cpDescription = nftTestTokenDescription
        , cpAuthor = nftTestTokenAuthor
        , cpFile = nftTestTokenFile }
    Trace.callEndpoint @"create" hdl nftTokenParams
    void $ Trace.waitNSlots 5
    extractTokenMeta hdl

createNonMarketNftTokenTrace :: 
    Trace.ContractHandle (Maybe (Semigroup.Last TestNFTCurrency)) NFTCurrency.CurrencySchema Text
    -> TokenName 
    -> EmulatorTrace (NFTMetadataDto, NFTCurrency.TestNFTCurrency)
createNonMarketNftTokenTrace hdl tokenName = do
    let nftTokenForgeParams = NFTCurrency.ForgeNftParams { fnpTokenName = tokenName }
    Trace.callEndpoint @"create" hdl nftTokenForgeParams
    void $ Trace.waitNSlots 5
    testNftCur <- extractCurrencyForgedNFT hdl
    let metaDto = nftMetadataToDto $ createNftMeta tokenName $ currencySymbol testNftCur
    return (metaDto, testNftCur)

sellNftTokenTrace :: 
    Trace.ContractHandle (Last(Either Text MarketContractState)) MarketUserSchema Void  
    -> NFTMetadataDto 
    -> EmulatorTrace ()
sellNftTokenTrace hdl nftTokenMeta = do
    let nftTokenSellParams = NFTMarket.SellParams { spTokenSymbol = nftDtoTokenSymbol nftTokenMeta, spSellPrice = 1000}
    Trace.callEndpoint @"sell" hdl nftTokenSellParams
    void $ Trace.waitNSlots 5

cancelSellNftTokenTrace :: 
    Trace.ContractHandle (Last(Either Text MarketContractState)) MarketUserSchema Void  
    -> NFTMetadataDto 
    -> EmulatorTrace ()
cancelSellNftTokenTrace hdl nftTokenMeta = do
    let nftTokenSellParams = NFTMarket.CancelSellParams { cspTokenSymbol = nftDtoTokenSymbol nftTokenMeta }
    Trace.callEndpoint @"cancelSell" hdl nftTokenSellParams
    void $ Trace.waitNSlots 5

buyNftTokenTrace :: 
    Trace.ContractHandle (Last(Either Text MarketContractState)) MarketUserSchema Void   
    -> NFTMetadataDto 
    -> EmulatorTrace ()
buyNftTokenTrace hdl nftTokenMeta = do
    let nftTokenBuyParams = NFTMarket.BuyParams { bpTokenSymbol = nftDtoTokenSymbol nftTokenMeta }
    Trace.callEndpoint @"buy" hdl nftTokenBuyParams
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