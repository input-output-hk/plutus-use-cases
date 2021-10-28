{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MonoLocalBinds     #-}

module Spec.NFT
    ( tests
    ) where

import           Contracts.NFT                          as NFTMarket
import           Control.Monad                          (void)
import           Control.Monad.Freer.Extras             as Extras
import qualified Control.Monad.Freer.Extras.Log         as Log
import qualified Data.ByteString.Base64                 as B64
import qualified Data.ByteString.Char8                  as B
import           Data.Monoid                            (Last (..))
import qualified Data.Semigroup                         as Semigroup
import           Data.String                            (fromString)
import           Data.Text                              (Text, pack)
import           Data.Void                              (Void)
import           Ledger                                 (PubKeyHash(..), pubKeyHash)
import           Ledger.Ada                             as Ada
import           Ledger.Index                           (ValidationError (ScriptFailure))
import           Ledger.Scripts                         (ScriptError (EvaluationError))
import           Ledger.Value                           (CurrencySymbol(..), TokenName (..), assetClassValue, valueOf)
import           Plutus.Contract                        hiding (when)
import           Plutus.Contract.Test
import           Plutus.Trace.Emulator                  (EmulatorTrace)
import qualified Plutus.Trace.Emulator                  as Trace
import           Plutus.Trace.Emulator.Types  
import           PlutusTx.Prelude                       as PlutusTx
import           Plutus.V1.Ledger.Value                 (singleton)
import qualified Prelude
import           Prelude                                (read, show)
import           Test.Tasty
import           Spec.MockNFTCurrency                   as MockCurrency 
import           Spec.Helper

ownerWallet :: Wallet
ownerWallet = w5

t1, t2 :: ContractInstanceTag
t1 = Trace.walletInstanceTag w1
t2 = Trace.walletInstanceTag w2

ownerContract :: Contract (Last (Either Text NFTMarket)) MarketOwnerSchema ContractError ()
ownerContract = NFTMarket.ownerEndpoint forgeMockNftToken $ marketFee nftMarketMock 

userContract :: Promise (Last (Either Text MarketContractState)) MarketUserSchema Void ()
userContract = NFTMarket.userEndpoints nftMarketMock

tests :: TestTree
tests = testGroup "nft"
    [
        -- checkPredicate "Owner contract expose 'start' endpoints"
        -- (
        --     endpointAvailable @"start" ownerContract t1
        -- )
        -- activeOwnerContractTrace
        -- ,
        checkPredicate "Should create NFT token"
        ( 
            assertNoFailedTransactions
            .&&. valueAtAddress (marketAddress nftMarketMock) 
                (== (assetClassValue (testTokenMetaClass testToken1) 1 
                    <> assetClassValue (marketId nftMarketMock) 1)
                )
            .&&. walletFundsChange w1 (assetClassValue (testTokenClass testToken1) 1)
            .&&. assertAccumState userContract t1 
                (\case Last (Just (Right (NFTMarket.Created meta))) -> meta == testToken1MetaDto; _ -> False) 
                "should create NFT state"
        )
        createNftTokenFlowTrace
        ,
        checkPredicate "Should fail if duplicate token created"
        ( 
            assertFailedTransaction (\_ err _ -> case err of {ScriptFailure (EvaluationError ["nft token is arleady exists", "PT5"] _) -> True; _ -> False  })
        )
        createDuplicateNftTokenFailureTrace
        ,
        checkPredicate "Should start sell NFT token"
        ( 
            assertNoFailedTransactions
            .&&. valueAtAddress (marketAddress nftMarketMock)
                (== (assetClassValue (testTokenClass testToken1) 1 
                    <> assetClassValue (testTokenMetaClass testToken1)  1 
                    <> assetClassValue (marketId nftMarketMock) 1))
            -- create and send token in one trace
            .&&. walletFundsChange w1 (Ada.lovelaceValueOf 0)
            .&&. assertAccumState userContract t1
            (\case Last (Just (Right (NFTMarket.Selling meta))) -> 
                        meta == (toMetaDto $ makeSellingTestToken testToken1 w1 nftMaketSellPrice);
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
        checkPredicate "Should fail start selling if price less than 0"
        ( 
            assertNoFailedTransactions
            .&&. assertAccumState userContract t1
            (\case Last (Just (Left errText)) -> 
                    errText Prelude.== pack "sell price should be greater than zero";
                    _ -> False) 
                "should have failed state"
        )
        sellFailureOnLessThanZeroPriceTrace
        ,
        checkPredicate "Should fail start selling if price less than fee"
        ( 
            assertFailedTransaction (\_ err _ -> case err of {ScriptFailure (EvaluationError ["price should be greater than fee", "PT5"] _) -> True; _ -> False  })
        )
        sellFailureOnLessThanFeePriceTrace
        ,
        checkPredicate "Should cancel NFT token selling"
        ( 
            assertNoFailedTransactions
            .&&. valueAtAddress (marketAddress nftMarketMock)
                (== (assetClassValue (testTokenMetaClass testToken1) 1 
                    <> assetClassValue (marketId nftMarketMock) 1))
            -- create and send token in one trace
            .&&. walletFundsChange w1 (assetClassValue (testTokenClass testToken1) 1)
            .&&. assertAccumState userContract t1
            (\case Last (Just (Right (NFTMarket.CancelSelling meta))) -> 
                         meta == testToken1MetaDto;
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
                "should have failed state"
        )
        cancelSellFailureIfNotSaleTrace
        ,
        checkPredicate "Should fail cancel sell if not token owner"
            ( 
                assertFailedTransaction (\_ err _ -> case err of {ScriptFailure (EvaluationError ["owner should sign", "PT5"] _) -> True; _ -> False  })
            )
        cancelSellFailureIfNotOwnerTrace
        ,
        checkPredicate "Should buy NFT token"
        ( 
           assertNoFailedTransactions
           .&&. valueAtAddress (marketAddress nftMarketMock) 
                (== (assetClassValue (testTokenMetaClass testToken1) 1 
                    <> assetClassValue (marketId nftMarketMock) 1))
           .&&. walletFundsChange w1 (Ada.lovelaceValueOf (nftMaketSellPrice - nftMarketFee))
           .&&. walletFundsChange w2 (Ada.lovelaceValueOf (negate (nftMaketSellPrice)) <> assetClassValue (testTokenClass testToken1) 1)
           .&&. walletFundsChange ownerWallet (Ada.lovelaceValueOf nftMarketFee)
           .&&. assertAccumState userContract t2
                (\case Last (Just (Right (NFTMarket.Buyed meta))) -> meta == testToken1MetaDto; _ -> False) 
                "should create buy NFT state"
        )
        buyNftTokenFlowTrace
        ,
        checkPredicate "Should not buy NFT token if not on sale"
        ( 
            assertNoFailedTransactions
            .&&. assertAccumState userContract t2
            (\case Last (Just (Left errText)) -> 
                    errText Prelude.== pack "NFT token is not on sale";
                    _ -> False) 
                "should have failed state"
        )
        buyNftNotOnSaleFailureFlowTrace
        ,
        let 
            expectedMeta1 = toMetaDto $ makeSellingTestToken testToken1 w1 nftMaketSellPrice
            expectedMeta2 = toMetaDto $ makeSellingTestToken testToken2 w2 nftMaketSellPrice
        in
        checkPredicate "Should get selling tokens"
        ( 
            assertNoFailedTransactions
            .&&. assertAccumState userContract t1
            (\case Last (Just (Right (NFTMarket.SellingTokens metas))) -> metas == [expectedMeta1, expectedMeta2]; _ -> False) 
                "should have selling nft tokens in state"
        )
        shouldShowAllSellingTokensTrace
        ,
        checkPredicate "Should get owned only nft tokens"
        ( 
            assertNoFailedTransactions
            .&&. assertAccumState userContract t1
            (\case Last (Just (Right (NFTMarket.Tokens metas))) -> metas == [testToken1MetaDto , testToken2MetaDto]; 
                   _ -> False) 
                "should have owned only nft token in state"
        )
        shouldGetOwnerNftTokensTrace
        ,
        checkPredicate "Should get only onwer tokens"
        ( 
            assertNoFailedTransactions
            .&&. assertAccumState userContract t1
            (\case Last (Just (Right (NFTMarket.Tokens metas))) -> metas == [testToken1MetaDto]; 
                   _ -> False) 
                "should have only owned nft token in state"
        )
        shouldNotGetOtherWalletTokensTrace
        ,
        let 
            expectedMeta2 = toMetaDto $ makeSellingTestToken testToken2 w1 nftMaketSellPrice
        in
        checkPredicate "Should get owned and selling nft tokens"
        ( 
            assertNoFailedTransactions
            .&&. assertAccumState userContract t1
            (\case Last (Just (Right (NFTMarket.Tokens metas))) -> metas == [testToken1MetaDto, expectedMeta2]; 
                   _ -> False) 
                "should have owned and salling nft token in state"
        )
        shouldGetOwnerNftTokensWithOnSaleTraces
        ,
        checkPredicate "Should get wallet key"
        ( 
            assertNoFailedTransactions
            .&&. assertAccumState userContract t1
            (\case Last (Just (Right (NFTMarket.UserPubKeyHash keyHash))) -> 
                        keyHash Prelude.== (B.unpack . B64.encode . fromBuiltin . getPubKeyHash . pubKeyHash . walletPubKey $ w1); 
                   _ -> False) 
                "should get wallet key state"
        )
        shouldGetWalletKeyTrace
        ,
        checkPredicate "Should get wallet info"
        ( 
            assertNoFailedTransactions
            .&&. assertAccumState userContract t1
            (\case Last (Just (Right (NFTMarket.WalletInfo walletInfo))) -> 
                        valueOf walletInfo adaCurrency adaName == valueOf walletMockData adaCurrency adaName &&
                        valueOf walletInfo (testTokenSymbol testToken1) (testTokenName testToken1) ==  valueOf walletMockData (testTokenSymbol testToken1) (testTokenName testToken1)
                   _ -> False)
                "should get wallet info state"
        )
        shouldGetWalletInfoTrace
    ]

initialise :: EmulatorTrace ()
initialise = do
    -- need to have separate wallet for start contract
    -- https://github.com/input-output-hk/plutus/issues/3359
    ownerHdl <- Trace.activateContractWallet ownerWallet ownerContract
    Trace.callEndpoint @"start" ownerHdl ()
    void $ Trace.waitNSlots 10

createNftTokenFlowTrace :: EmulatorTrace ()
createNftTokenFlowTrace = do
    initialise
    user1Hdl <- Trace.activateContractWallet w1 userContract
    void $ createNftTokenTrace user1Hdl testToken1

createDuplicateNftTokenFailureTrace :: EmulatorTrace ()
createDuplicateNftTokenFailureTrace = do
    initialise
    user1Hdl <- Trace.activateContractWallet w1 userContract
    void $ createNftTokenTrace user1Hdl testToken1
    void $ createNftTokenTrace user1Hdl testToken1

sellNftTokenFlowTrace :: EmulatorTrace ()
sellNftTokenFlowTrace = do
    initialise
    user1Hdl <- Trace.activateContractWallet w1 userContract
    nftTokenMeta <- createNftTokenTrace user1Hdl testToken1
    sellNftTokenTrace user1Hdl nftTokenMeta nftMaketSellPrice

sellNonMarketNFTFailureTrace  :: EmulatorTrace ()
sellNonMarketNFTFailureTrace = do
    initialise
    forgeCurrencyHdl <- Trace.activateContract w1 MockCurrency.forgeNftToken (fromString $ "forgeCurrency: " <> show t1)
    nonMarketNftMeta <- createNonMarketNftTokenTrace forgeCurrencyHdl nonMarketToken1
    user1Hdl <- Trace.activateContractWallet w1 userContract
    sellNftTokenTrace user1Hdl nonMarketNftMeta nftMaketSellPrice

sellFailureOnLessThanZeroPriceTrace  :: EmulatorTrace ()
sellFailureOnLessThanZeroPriceTrace = do
    initialise
    user1Hdl <- Trace.activateContractWallet w1 userContract
    nftTokenMeta <- createNftTokenTrace user1Hdl testToken1
    sellNftTokenTrace user1Hdl nftTokenMeta 0

sellFailureOnLessThanFeePriceTrace  :: EmulatorTrace ()
sellFailureOnLessThanFeePriceTrace = do
    initialise
    user1Hdl <- Trace.activateContractWallet w1 userContract
    nftTokenMeta <- createNftTokenTrace user1Hdl testToken1
    sellNftTokenTrace user1Hdl nftTokenMeta sellPriceLowerThanFee

cancelSellNftTokenFlowTrace :: EmulatorTrace ()
cancelSellNftTokenFlowTrace = do
    initialise
    user1Hdl <- Trace.activateContractWallet w1 userContract
    nftTokenMeta <- createNftTokenTrace user1Hdl testToken1
    sellNftTokenTrace user1Hdl nftTokenMeta nftMaketSellPrice
    cancelSellNftTokenTrace user1Hdl nftTokenMeta

cancelSellFailureIfNotSaleTrace :: EmulatorTrace ()
cancelSellFailureIfNotSaleTrace = do
    initialise
    user1Hdl <- Trace.activateContractWallet w1 userContract
    nftTokenMeta <- createNftTokenTrace user1Hdl testToken1
    cancelSellNftTokenTrace user1Hdl nftTokenMeta

cancelSellFailureIfNotOwnerTrace :: EmulatorTrace ()
cancelSellFailureIfNotOwnerTrace = do
    initialise
    user1Hdl <- Trace.activateContractWallet w1 userContract
    user2Hdl <- Trace.activateContractWallet w2 userContract
    nftTokenMeta <- createNftTokenTrace user1Hdl testToken1
    sellNftTokenTrace user1Hdl nftTokenMeta nftMaketSellPrice
    cancelSellNftTokenTrace user2Hdl nftTokenMeta

buyNftTokenFlowTrace :: EmulatorTrace ()
buyNftTokenFlowTrace = do
    initialise
    user1Hdl <- Trace.activateContractWallet w1 userContract
    user2Hdl <- Trace.activateContractWallet w2 userContract
    nftTokenMeta <- createNftTokenTrace user1Hdl testToken1
    sellNftTokenTrace user1Hdl nftTokenMeta nftMaketSellPrice
    buyNftTokenTrace user2Hdl nftTokenMeta

buyNftNotOnSaleFailureFlowTrace :: EmulatorTrace ()
buyNftNotOnSaleFailureFlowTrace = do
    initialise
    user1Hdl <- Trace.activateContractWallet w1 userContract
    user2Hdl <- Trace.activateContractWallet w2 userContract
    nftTokenMeta <- createNftTokenTrace user1Hdl testToken1
    buyNftTokenTrace user2Hdl nftTokenMeta

shouldShowAllSellingTokensTrace :: EmulatorTrace ()
shouldShowAllSellingTokensTrace = do
    initialise
    user1Hdl <- Trace.activateContractWallet w1 userContract
    user2Hdl <- Trace.activateContractWallet w2 userContract
    nftTokenMeta1 <- createNftTokenTrace user1Hdl testToken1
    nftTokenMeta2 <- createNftTokenTrace user2Hdl testToken2
    _ <- createNftTokenTrace user1Hdl testToken3
    sellNftTokenTrace user1Hdl nftTokenMeta1 nftMaketSellPrice
    sellNftTokenTrace user2Hdl nftTokenMeta2 nftMaketSellPrice
    Trace.callEndpoint @"sellingTokens" user1Hdl ()
    void $ Trace.waitNSlots 5

shouldGetOwnerNftTokensTrace :: EmulatorTrace ()
shouldGetOwnerNftTokensTrace = do
    initialise
    user1Hdl <- Trace.activateContractWallet w1 userContract
    _ <- createNftTokenTrace user1Hdl testToken1
    _ <-createNftTokenTrace user1Hdl testToken2
    forgeCurrencyHdl <- Trace.activateContract w1 MockCurrency.forgeNftToken (fromString $ "forgeCurrency: " <> show t1)
    _ <- createNonMarketNftTokenTrace forgeCurrencyHdl nonMarketToken1
    Trace.callEndpoint @"userNftTokens" user1Hdl ()
    void $ Trace.waitNSlots 5

shouldNotGetOtherWalletTokensTrace :: EmulatorTrace ()
shouldNotGetOtherWalletTokensTrace = do
    initialise
    user1Hdl <- Trace.activateContractWallet w1 userContract
    user2Hdl <- Trace.activateContractWallet w2 userContract
    _ <- createNftTokenTrace user1Hdl testToken1
    _ <- createNftTokenTrace user2Hdl testToken3
    Trace.callEndpoint @"userNftTokens" user1Hdl ()
    void $ Trace.waitNSlots 5

shouldGetOwnerNftTokensWithOnSaleTraces :: EmulatorTrace ()
shouldGetOwnerNftTokensWithOnSaleTraces = do
    initialise
    user1Hdl <- Trace.activateContractWallet w1 userContract
    _ <- createNftTokenTrace user1Hdl testToken1
    nftTokenMeta2 <- createNftTokenTrace user1Hdl testToken2
    sellNftTokenTrace user1Hdl nftTokenMeta2 nftMaketSellPrice
    Trace.callEndpoint @"userNftTokens" user1Hdl ()
    void $ Trace.waitNSlots 5

shouldGetWalletKeyTrace :: EmulatorTrace ()
shouldGetWalletKeyTrace = do
    initialise
    user1Hdl <- Trace.activateContractWallet w1 userContract
    Trace.callEndpoint @"userPubKeyHash" user1Hdl ()
    void $ Trace.waitNSlots 5

shouldGetWalletInfoTrace :: EmulatorTrace ()
shouldGetWalletInfoTrace = do
    initialise
    user1Hdl <- Trace.activateContractWallet w1 userContract
    void $ createNftTokenTrace user1Hdl testToken1
    void $ Trace.waitNSlots 5
    Trace.callEndpoint @"walletInfo" user1Hdl ()
    void $ Trace.waitNSlots 5

forgeMockNftToken:: 
    forall w s. TokenName
    -> PubKeyHash
    -> Contract w s Text CurrencySymbol
forgeMockNftToken tokenName pk = 
    MockCurrency.currencySymbol 
    <$> MockCurrency.forgeContract pk tokenName

activeOwnerContractTrace :: EmulatorTrace ()
activeOwnerContractTrace = void $ Trace.activateContractWallet w1 ownerContract

createNftTokenTrace :: 
    Trace.ContractHandle (Last(Either Text MarketContractState)) MarketUserSchema Void 
    -> TestTokenMeta 
    -> EmulatorTrace NFTMetadataDto
createNftTokenTrace hdl testToken = do
    let nftTokenParams = NFTMarket.CreateParams { 
        cpTokenName = read . show $ testTokenName testToken
        , cpDescription = testTokenDesciption testToken
        , cpAuthor = testTokenAuthor testToken
        , cpFile = testTokenFile testToken }
    Trace.callEndpoint @"create" hdl nftTokenParams
    void $ Trace.waitNSlots 5
    extractTokenMeta hdl

createNonMarketNftTokenTrace :: 
    Trace.ContractHandle (Maybe (Semigroup.Last MockNFTCurrency)) MockCurrency.CurrencySchema Text
    -> TestTokenMeta 
    -> EmulatorTrace NFTMetadataDto
createNonMarketNftTokenTrace hdl tokenMeta = do
    let nftTokenForgeParams = MockCurrency.ForgeNftParams { MockCurrency.fnpTokenName = testTokenName tokenMeta }
    Trace.callEndpoint @"create" hdl nftTokenForgeParams
    void $ Trace.waitNSlots 5
    _ <- extractCurrencyForgedNFT hdl
    let metaDto = nftMetadataToDto $ createNftMeta $ tokenMeta
    return metaDto

sellNftTokenTrace :: 
    Trace.ContractHandle (Last(Either Text MarketContractState)) MarketUserSchema Void  
    -> NFTMetadataDto
    -> Integer
    -> EmulatorTrace ()
sellNftTokenTrace hdl nftTokenMeta sellPrice = do
    let nftTokenSellParams = NFTMarket.SellParams { spTokenName = nftDtoTokenName nftTokenMeta, spSellPrice = sellPrice}
    Trace.callEndpoint @"sell" hdl nftTokenSellParams
    void $ Trace.waitNSlots 5

cancelSellNftTokenTrace :: 
    Trace.ContractHandle (Last(Either Text MarketContractState)) MarketUserSchema Void  
    -> NFTMetadataDto 
    -> EmulatorTrace ()
cancelSellNftTokenTrace hdl nftTokenMeta = do
    let nftTokenSellParams = NFTMarket.CancelSellParams { cspTokenName = nftDtoTokenName nftTokenMeta }
    Trace.callEndpoint @"cancelSell" hdl nftTokenSellParams
    void $ Trace.waitNSlots 5

buyNftTokenTrace :: 
    Trace.ContractHandle (Last(Either Text MarketContractState)) MarketUserSchema Void   
    -> NFTMetadataDto 
    -> EmulatorTrace ()
buyNftTokenTrace hdl nftTokenMeta = do
    let nftTokenBuyParams = NFTMarket.BuyParams { bpTokenName = nftDtoTokenName nftTokenMeta }
    Trace.callEndpoint @"buy" hdl nftTokenBuyParams
    void $ Trace.waitNSlots 5