{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Marketplace.Spec.Sale
  ( tests
  ) where

import           Control.Lens                                 (_2, _Left, (&),
                                                               (^.), (^?))
import           Control.Monad                                (void)
import           Data.Foldable                                (find)
import           Data.Maybe                                   (isNothing)
import           Data.Proxy
import           Data.Text                                    (Text)
import           Data.Void                                    (Void)
import           Ledger.Ada                                   (lovelaceValueOf)
import qualified Ledger.Value                                 as V
import qualified Marketplace.Fixtures                         as Fixtures
import qualified Marketplace.Spec.Bundles                     as Bundles
import qualified Marketplace.Spec.CreateNft                   as CreateNft
import qualified Marketplace.Spec.Start                       as Start
import           Plutus.Abstract.ContractResponse             (ContractResponse)
import           Plutus.Contract.Test
import qualified Plutus.Contracts.NftMarketplace.Endpoints    as Marketplace
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Marketplace
import qualified Plutus.Contracts.Services.Sale               as Sale
import qualified Plutus.Trace                                 as Trace
import qualified PlutusTx.AssocMap                            as AssocMap
import           Test.Tasty
import qualified Utils
import           Wallet.Emulator.Wallet                       (walletAddress)
import           Ledger.Ada                                   (Ada(..), toValue)
import Ledger.Index (minAdaTxOut)

tests :: TestTree
tests =
  testGroup
    "Sale"
  [testGroup
    "NFT singletons"
    [
      checkPredicateOptions
        Fixtures.options
        "Should put on sale one NFT locking it in sale script & saving link"
        (openSaleValueCheck .&&. openSaleDatumsCheck)
        (void openSaleTrace),
      checkPredicateOptions
        Fixtures.options
        "Should not put on sale if NFT does not exist"
        errorCheckOpen
        openSaleTrace',
      checkPredicateOptions
        Fixtures.options
        "Should close sale and pay locked NFT back"
        (closeSaleValueCheck .&&. completeSaleDatumsCheck)
        closeSaleTrace,
      checkPredicateOptions
        Fixtures.options
        "Should not close sale if it was not started"
        errorCheckClose
        closeSaleTrace',
      checkPredicateOptions
        Fixtures.options
        "Should sell NFT and pay the token to buyer"
        (buyItemValueCheck .&&. completeSaleDatumsCheck)
        buyItemTrace,
      checkPredicateOptions
        Fixtures.options
        "Should not sell NFT if it has no lot"
        errorCheckBuyer
        buyItemTrace'
      -- checkPredicateOptions
      --   Fixtures.options
      --   "Should sell NFT and pay fee to marketplace operator"
      --   (marketplaceOperatorFeeCheck .&&. sellersProfitWithPayingFeeCheck)
      --   buyItemTrace
    ]]
  --   ,
  -- testGroup
  --   "NFT bundles"
  --   [
  --     checkPredicateOptions
  --       Fixtures.options
  --       "Should put on sale NFT bundle locking bundle value in sale script & saving link"
  --       (openSaleValueCheckB .&&. openSaleDatumsCheckB)
  --       (void openSaleTraceB),
  --     checkPredicateOptions
  --       Fixtures.options
  --       "Should not put on sale if bundle does not exist"
  --       errorCheckOpen
  --       openSaleTraceB',
  --     checkPredicateOptions
  --       Fixtures.options
  --       "Should close sale and pay locked bundle value back"
  --       (closeSaleValueCheckB .&&. completeSaleDatumsCheckB)
  --       closeSaleTraceB,
  --     checkPredicateOptions
  --       Fixtures.options
  --       "Should sell bundle and pay its value to buyer"
  --       (buyItemValueCheckB .&&. completeSaleDatumsCheckB)
  --       buyItemTraceB,
  --     checkPredicateOptions
  --       Fixtures.options
  --       "Should sell bundle and pay fee to marketplace operator"
  --       (marketplaceOperatorFeeCheckB .&&. sellersProfitWithPayingFeeCheckB)
  --       buyItemTraceB
  --   ]]

singletonNftPrice :: Integer
singletonNftPrice = 60 * Fixtures.oneAdaInLovelace

-- \/\/\/ "NFT singletons"
openSaleParams ::        Marketplace.OpenSaleParams
openSaleParams =  Marketplace.OpenSaleParams {
                    Marketplace.ospItemId   = Marketplace.UserNftId Fixtures.catTokenIpfsCid,
                    Marketplace.ospSalePrice = singletonNftPrice
                   }

closeLotParams ::        Marketplace.CloseLotParams
closeLotParams =  Marketplace.CloseLotParams {
                      Marketplace.clpItemId    = Marketplace.UserNftId Fixtures.catTokenIpfsCid
                    }

openSaleTrace :: Trace.EmulatorTrace (Trace.ContractHandle (ContractResponse String Text Marketplace.UserContractState) Marketplace.MarketplaceUserSchema Void)
openSaleTrace = do
  h <- CreateNft.createNftTrace

  _ <- Trace.callEndpoint @"openSale" h openSaleParams

  _ <- Trace.waitNSlots 50
  pure h

openSaleTrace' :: Trace.EmulatorTrace ()
openSaleTrace' = do
  _ <- Start.startTrace
  h <- Trace.activateContractWallet Fixtures.userWallet $ Marketplace.userEndpoints Fixtures.marketplace

  _ <- Trace.callEndpoint @"openSale" h openSaleParams

  _ <- Trace.waitNSlots 50
  pure ()

closeSaleTrace :: Trace.EmulatorTrace ()
closeSaleTrace = do
  h <- openSaleTrace

  _ <- Trace.callEndpoint @"closeSale" h closeLotParams

  _ <- Trace.waitNSlots 50
  pure ()

closeSaleTrace' :: Trace.EmulatorTrace ()
closeSaleTrace' = do
  h <- CreateNft.createNftTrace

  _ <- Trace.callEndpoint @"closeSale" h closeLotParams

  _ <- Trace.waitNSlots 50
  pure ()

buyItemTrace :: Trace.EmulatorTrace ()
buyItemTrace = do
  _ <- openSaleTrace

  h <- Trace.activateContractWallet Fixtures.buyerWallet $ Marketplace.userEndpoints Fixtures.marketplace
  _ <- Trace.callEndpoint @"buyItem" h closeLotParams

  _ <- Trace.waitNSlots 50
  pure ()

buyItemTrace' :: Trace.EmulatorTrace ()
buyItemTrace' = do
  _ <- CreateNft.createNftTrace

  h <- Trace.activateContractWallet Fixtures.buyerWallet $ Marketplace.userEndpoints Fixtures.marketplace
  _ <- Trace.callEndpoint @"buyItem" h closeLotParams

  _ <- Trace.waitNSlots 50
  pure ()

openSaleDatumsCheck :: TracePredicate
openSaleDatumsCheck =
  dataAtAddress
    Fixtures.marketplaceAddress
    (Utils.checkOneDatum (nftIsOnSale . Marketplace.mdSingletons))
    where
      nftIsOnSale = maybe False (\t -> Marketplace.getSaleFromNFT t & fmap Sale.saleValue &
                                (== Just (Marketplace.nftValue Fixtures.catTokenIpfsCidBs t))) .
                    AssocMap.lookup Fixtures.catTokenIpfsCidHash

completeSaleDatumsCheck :: TracePredicate
completeSaleDatumsCheck =
  dataAtAddress
    Fixtures.marketplaceAddress
    (Utils.checkOneDatum (nftNotOnSale . Marketplace.mdSingletons))
    where
      nftNotOnSale = maybe False (isNothing . Marketplace.nftLot) .
                     AssocMap.lookup Fixtures.catTokenIpfsCidHash

openSaleValueCheck :: TracePredicate
openSaleValueCheck =
  valueAtAddress
    (walletAddress Fixtures.userWallet)
    (isNothing . find hasNft . V.flattenValue)
    where
      hasNft v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCidBs

closeSaleValueCheck :: TracePredicate
closeSaleValueCheck =
  valueAtAddress
    (walletAddress Fixtures.userWallet)
    (Utils.one hasNft . V.flattenValue)
    where
      hasNft v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCidBs

buyItemValueCheck :: TracePredicate
buyItemValueCheck =
  valueAtAddress
    (walletAddress Fixtures.buyerWallet)
    (Utils.one hasNft . V.flattenValue)
    where
      hasNft v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCidBs

errorCheckOpen :: TracePredicate
errorCheckOpen = Utils.assertCrError (Proxy @"openSale") (Marketplace.userEndpoints Fixtures.marketplace) (Trace.walletInstanceTag Fixtures.userWallet)

errorCheckClose :: TracePredicate
errorCheckClose = Utils.assertCrError (Proxy @"closeSale") (Marketplace.userEndpoints Fixtures.marketplace) (Trace.walletInstanceTag Fixtures.userWallet)

errorCheckBuyer :: TracePredicate
errorCheckBuyer = Utils.assertCrError (Proxy @"buyItem") (Marketplace.userEndpoints Fixtures.marketplace) (Trace.walletInstanceTag Fixtures.buyerWallet)

-- \/\/\/ "NFT bundles"
openSaleParamsB ::        Marketplace.OpenSaleParams
openSaleParamsB =  Marketplace.OpenSaleParams {
                    Marketplace.ospItemId   = Marketplace.UserBundleId Fixtures.cids,
                    Marketplace.ospSalePrice = 65 * Fixtures.oneAdaInLovelace
                   }

closeLotParamsB ::        Marketplace.CloseLotParams
closeLotParamsB =  Marketplace.CloseLotParams {
                      Marketplace.clpItemId    = Marketplace.UserBundleId Fixtures.cids
                    }

openSaleTraceB :: Trace.EmulatorTrace (Trace.ContractHandle (ContractResponse String Text Marketplace.UserContractState) Marketplace.MarketplaceUserSchema Void)
openSaleTraceB = do
  h <- Bundles.bundleTrace

  _ <- Trace.callEndpoint @"openSale" h openSaleParamsB

  _ <- Trace.waitNSlots 50
  pure h

openSaleTraceB' :: Trace.EmulatorTrace ()
openSaleTraceB' = do
  _ <- Start.startTrace
  h <- Trace.activateContractWallet Fixtures.userWallet $ Marketplace.userEndpoints Fixtures.marketplace

  _ <- Trace.callEndpoint @"openSale" h openSaleParamsB

  _ <- Trace.waitNSlots 50
  pure ()

closeSaleTraceB :: Trace.EmulatorTrace ()
closeSaleTraceB = do
  h <- openSaleTraceB

  _ <- Trace.callEndpoint @"closeSale" h closeLotParamsB

  _ <- Trace.waitNSlots 50
  pure ()

buyItemTraceB :: Trace.EmulatorTrace ()
buyItemTraceB = do
  _ <- openSaleTraceB

  h <- Trace.activateContractWallet Fixtures.buyerWallet $ Marketplace.userEndpoints Fixtures.marketplace
  _ <- Trace.callEndpoint @"buyItem" h closeLotParamsB

  _ <- Trace.waitNSlots 50
  pure ()

openSaleDatumsCheckB :: TracePredicate
openSaleDatumsCheckB =
  dataAtAddress
    Fixtures.marketplaceAddress
    (Utils.checkOneDatum (bundleIsOnSale . Marketplace.mdBundles))
    where
      bundleIsOnSale = maybe False (\b -> Marketplace.getSaleFromBundle b & fmap Sale.saleValue &
                                (== Just (Marketplace.bundleValue AssocMap.empty b))) .
                       AssocMap.lookup Fixtures.bundleId

completeSaleDatumsCheckB :: TracePredicate
completeSaleDatumsCheckB =
  dataAtAddress
    Fixtures.marketplaceAddress
    (Utils.checkOneDatum (bundleNotOnSale . Marketplace.mdBundles))
    where
      bundleNotOnSale = maybe False (Prelude.not . Marketplace.hasLotBundle) .
                        AssocMap.lookup Fixtures.bundleId

openSaleValueCheckB :: TracePredicate
openSaleValueCheckB =
  valueAtAddress
    (walletAddress Fixtures.userWallet) $
    \v -> (isNothing . find hasCatToken . V.flattenValue $ v) && (isNothing . find hasPhotoToken . V.flattenValue $ v)
    where
      hasCatToken v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCidBs
      hasPhotoToken v = (v ^. _2 & V.unTokenName) == Fixtures.photoTokenIpfsCidBs

closeSaleValueCheckB :: TracePredicate
closeSaleValueCheckB =
  valueAtAddress
    (walletAddress Fixtures.userWallet) $
    \v -> (Utils.one hasCatToken . V.flattenValue $ v) && (Utils.one hasPhotoToken . V.flattenValue $ v)
    where
      hasCatToken v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCidBs
      hasPhotoToken v = (v ^. _2 & V.unTokenName) == Fixtures.photoTokenIpfsCidBs

buyItemValueCheckB :: TracePredicate
buyItemValueCheckB =
  valueAtAddress
    (walletAddress Fixtures.buyerWallet) $
    \v -> (Utils.one hasCatToken . V.flattenValue $ v) && (Utils.one hasPhotoToken . V.flattenValue $ v)
    where
      hasCatToken v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCidBs
      hasPhotoToken v = (v ^. _2 & V.unTokenName) == Fixtures.photoTokenIpfsCidBs

marketplaceOperatorFeeCheck :: TracePredicate
marketplaceOperatorFeeCheck =
  walletFundsChange Fixtures.ownerWallet $ toValue (totalMintingFee + saleFee - minAdaTxOut)
  -- 60_000_000 * 3.5 /100 = 2_100_000 - fee by complete sale
  -- 2_100_000 - fee by minting token
  -- 2_000_000 - minAdaTxOut by starting marketplace
  -- (2_100_000 + 2_100_000 - 2_000_000) = 2_200_000
  where
    totalMintingFee = Fixtures.marketplaceCreationFee
    saleFee = Lovelace $ Fixtures.roundedPercentage singletonNftPrice

sellersProfitWithPayingFeeCheck :: TracePredicate
sellersProfitWithPayingFeeCheck =
  walletFundsChange Fixtures.userWallet $ toValue (nftPriceAda - saleFee - totalMintingFee)
  -- 44000000 - 1200000 = 42800000 - seller's profit
  where
    saleFee = Lovelace $ Fixtures.roundedPercentage singletonNftPrice
    totalMintingFee = Fixtures.marketplaceCreationFee
    nftPriceAda = Lovelace singletonNftPrice

marketplaceOperatorFeeCheckB :: TracePredicate
marketplaceOperatorFeeCheckB =
  walletFundsChange Fixtures.ownerWallet $ lovelaceValueOf 1925000
  -- 65_000_000 * 3.5 /100 = 1_625_000 - fee by complete sale
  -- 100_000 * 2 = 200000 - fee by minting 2 tokens
  -- 100_000 - fee by bundling
  where
    totalMintingFee = Fixtures.marketplaceCreationFee * 2
    totalBundlingFee = Fixtures.marketplaceCreationFee
    

sellersProfitWithPayingFeeCheckB :: TracePredicate
sellersProfitWithPayingFeeCheckB =
  walletFundsChange Fixtures.userWallet $ lovelaceValueOf 63075000
  -- 65000000 - 1925000 = 63075000
