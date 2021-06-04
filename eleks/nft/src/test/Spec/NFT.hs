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

-- ownerConract :: Contract (Last (Either Text NFTMarket)) BlockchainActions Void ()
-- ownerConract = NFTMarket.ownerEndpoint
-- userContract :: NFTMarket -> Contract (Last (Either Text MarketContractState)) MarketUserSchema Void ()
-- userContract = NFTMarket.userEndpoints
-- 

tests :: TestTree
tests = testGroup "nft"
    [ --let con = void $ payEp @() @EscrowSchema @EscrowError escrowParams in
        checkPredicate "Should create NFT token and metadata"
        ( 
           assertNoFailedTransactions
           -- .&&. walletFundsChange (Wallet 1) (assetClassValue nftToken 1)
           -- .&&. (valueAtAddress stablecoinAddress (== (initialDeposit <> initialFee))
           .&&. walletFundsChange (Wallet 1) (Ada.lovelaceValueOf 1000)
           .&&. walletFundsChange (Wallet 2) (Ada.lovelaceValueOf (-1000) <> assetClassValue nftToken 1)
        )
        buyNftTokenTrace
    ]

forgeMockNftToken:: 
    forall w s. HasBlockchainActions s 
    => TokenName
    -> PubKeyHash
    -> Contract w s Text CurrencySymbol
forgeMockNftToken tokenName pk = 
    NFTCurrency.currencySymbol 
    <$> NFTCurrency.forgeContract pk tokenName

nftTokenName :: TokenName
nftTokenName = "testToken"
nftToken :: AssetClass
nftToken = AssetClass (NFTCurrency.currencySymbol $ TestNFTCurrency nftTokenName, nftTokenName)

nftTokenMetadataName :: TokenName
nftTokenMetadataName = "testTokenMetadata"
nftTokenMetadata :: AssetClass
nftTokenMetadata = AssetClass (NFTCurrency.currencySymbol $ TestNFTCurrency nftTokenMetadataName, nftTokenMetadataName)

buyNftTokenTrace :: EmulatorTrace ()
buyNftTokenTrace = do
    ownerHdl <- Trace.activateContractWallet w1 $ NFTMarket.ownerEndpoint (forgeMockNftToken :: TokenName -> PubKeyHash -> Contract (Last (Either Text NFTMarket)) MarketOwnerSchema Text CurrencySymbol)
    Trace.callEndpoint @"start" ownerHdl ()
    void $ Trace.waitNSlots 5
    market <- extractNFTMarket ownerHdl
    userMarketHdl1 <- Trace.activateContractWallet w1 $ NFTMarket.userEndpoints forgeMockNftToken market
    userMarketHdl2 <- Trace.activateContractWallet w2 $ NFTMarket.userEndpoints forgeMockNftToken market
    let nftTokenParams = NFTMarket.CreateParams { cpTokenName = "testToken", cpDescription = "TestDescrition", cpAuthor = "Author1", cpFile = "file1" }
    Trace.callEndpoint @"create" userMarketHdl1 nftTokenParams
    void $ Trace.waitNSlots 5

    nftTokenMeta <- extractTokenMeta userMarketHdl1
    let nftTokenSellParams = NFTMarket.SellParams { spTokenSymbol = nftDtoTokenSymbol nftTokenMeta, spSellPrice = 1000}
    Trace.callEndpoint @"sell" userMarketHdl1 nftTokenSellParams
    void $ Trace.waitNSlots 5

    let nftTokenBuyParams = NFTMarket.BuyParams { bpTokenSymbol = nftDtoTokenSymbol nftTokenMeta }
    Trace.callEndpoint @"buy" userMarketHdl2 nftTokenBuyParams
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