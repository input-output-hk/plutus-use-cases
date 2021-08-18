{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Marketplace.Spec.Sale
  ( tests
  ) where

import           Control.Lens                                 (_2, _Left, (&),
                                                               (.~), (^.), (^?))
import qualified Control.Lens                                 as Lens
import           Control.Monad                                (void)
import           Data.Foldable                                (find)
import           Data.Function                                (on)
import qualified Data.Map                                     as Map
import           Data.Maybe                                   (isNothing)
import           Data.Text                                    (Text)
import           Data.Void
import           Ledger
import qualified Ledger.Ada                                   as Ada
import qualified Ledger.Value                                 as V
import qualified Marketplace.Fixtures                         as Fixtures
import qualified Marketplace.Spec.Bundles                     as Bundles
import qualified Marketplace.Spec.CreateNft                   as CreateNft
import qualified Marketplace.Spec.Start                       as Start
import           Plutus.Abstract.ContractResponse
import           Plutus.Contract
import           Plutus.Contract.Test
import qualified Plutus.Contracts.NftMarketplace.Endpoints    as Marketplace
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Marketplace
import qualified Plutus.Contracts.Services.Sale               as Sale
import qualified Plutus.Trace                                 as Trace
import qualified PlutusTx.AssocMap                            as AssocMap
import           PlutusTx.Builtins                            (sha2_256)
import           PlutusTx.Prelude                             (ByteString)
import           Test.Tasty
import qualified Utils
import           Wallet.Emulator.Wallet

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
        errorCheckUser
        openSaleTrace',
      checkPredicateOptions
        Fixtures.options
        "Should close sale and pay locked NFT back"
        (closeSaleValueCheck .&&. completeSaleDatumsCheck)
        closeSaleTrace,
      checkPredicateOptions
        Fixtures.options
        "Should not close sale if it was not started"
        errorCheckUser
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
    ],
  testGroup
    "NFT bundles"
    [
      checkPredicateOptions
        Fixtures.options
        "Should put on sale NFT bundle locking bundle value in sale script & saving link"
        (openSaleValueCheckB .&&. openSaleDatumsCheckB)
        (void openSaleTraceB),
      checkPredicateOptions
        Fixtures.options
        "Should not put on sale if bundle does not exist"
        errorCheckUser
        openSaleTraceB',
      checkPredicateOptions
        Fixtures.options
        "Should close sale and pay locked bundle value back"
        (closeSaleValueCheckB .&&. completeSaleDatumsCheckB)
        closeSaleTraceB,
      checkPredicateOptions
        Fixtures.options
        "Should sell bundle and pay its value to buyer"
        (buyItemValueCheckB .&&. completeSaleDatumsCheckB)
        buyItemTraceB
    ]]

-- \/\/\/ "NFT singletons"
openSaleParams ::        Marketplace.OpenSaleParams
openSaleParams =  Marketplace.OpenSaleParams {
                    Marketplace.ospItemId   = Marketplace.UserNftId Fixtures.catTokenIpfsCid,
                    Marketplace.ospSalePrice = 44 * Fixtures.oneAdaInLovelace
                   }

closeLotParams ::        Marketplace.CloseLotParams
closeLotParams =  Marketplace.CloseLotParams {
                      Marketplace.clpItemId    = Marketplace.UserNftId Fixtures.catTokenIpfsCid
                    }

openSaleTrace :: Trace.EmulatorTrace (Trace.ContractHandle (ContractResponse Text Marketplace.UserContractState) Marketplace.MarketplaceUserSchema Void)
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
    (nftIsOnSale . Marketplace.mdSingletons)
    where
      nftIsOnSale = maybe False (\t -> t ^. Marketplace._nftLot ^? traverse . _2 . _Left & fmap Sale.saleValue &
                                (== Just (Marketplace.nftValue Fixtures.catTokenIpfsCid t))) .
                    (AssocMap.lookup Fixtures.catTokenIpfsCidHash)

completeSaleDatumsCheck :: TracePredicate
completeSaleDatumsCheck =
  dataAtAddress
    Fixtures.marketplaceAddress
    (nftNotOnSale . Marketplace.mdSingletons)
    where
      nftNotOnSale = maybe False (isNothing . Marketplace.nftLot) .
                     (AssocMap.lookup Fixtures.catTokenIpfsCidHash)

openSaleValueCheck :: TracePredicate
openSaleValueCheck =
  valueAtAddress
    (walletAddress Fixtures.userWallet)
    (isNothing . find hasNft . V.flattenValue)
    where
      hasNft v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCid

closeSaleValueCheck :: TracePredicate
closeSaleValueCheck =
  valueAtAddress
    (walletAddress Fixtures.userWallet)
    (Utils.one hasNft . V.flattenValue)
    where
      hasNft v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCid

buyItemValueCheck :: TracePredicate
buyItemValueCheck =
  valueAtAddress
    (walletAddress Fixtures.buyerWallet)
    (Utils.one hasNft . V.flattenValue)
    where
      hasNft v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCid

errorCheckUser :: TracePredicate
errorCheckUser = Utils.assertCrError (Marketplace.userEndpoints Fixtures.marketplace) (Trace.walletInstanceTag Fixtures.userWallet)

errorCheckBuyer :: TracePredicate
errorCheckBuyer = Utils.assertCrError (Marketplace.userEndpoints Fixtures.marketplace) (Trace.walletInstanceTag Fixtures.buyerWallet)

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

openSaleTraceB :: Trace.EmulatorTrace (Trace.ContractHandle (ContractResponse Text Marketplace.UserContractState) Marketplace.MarketplaceUserSchema Void)
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
    (bundleIsOnSale . Marketplace.mdBundles)
    where
      bundleIsOnSale = maybe False (\b -> b ^. Marketplace._nbTokens ^? Marketplace._HasLot . _2 . _Left & fmap Sale.saleValue &
                                (== Just (Marketplace.bundleValue AssocMap.empty b))) .
                       (AssocMap.lookup Fixtures.bundleId)

completeSaleDatumsCheckB :: TracePredicate
completeSaleDatumsCheckB =
  dataAtAddress
    Fixtures.marketplaceAddress
    (bundleNotOnSale . Marketplace.mdBundles)
    where
      bundleNotOnSale = maybe False (Prelude.not . Marketplace.hasLotBundle) .
                        (AssocMap.lookup Fixtures.bundleId)

openSaleValueCheckB :: TracePredicate
openSaleValueCheckB =
  valueAtAddress
    (walletAddress Fixtures.userWallet) $
    \v -> (isNothing . find hasCatToken . V.flattenValue $ v) && (isNothing . find hasPhotoToken . V.flattenValue $ v)
    where
      hasCatToken v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCid
      hasPhotoToken v = (v ^. _2 & V.unTokenName) == Fixtures.photoTokenIpfsCid

closeSaleValueCheckB :: TracePredicate
closeSaleValueCheckB =
  valueAtAddress
    (walletAddress Fixtures.userWallet) $
    \v -> (Utils.one hasCatToken . V.flattenValue $ v) && (Utils.one hasPhotoToken . V.flattenValue $ v)
    where
      hasCatToken v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCid
      hasPhotoToken v = (v ^. _2 & V.unTokenName) == Fixtures.photoTokenIpfsCid

buyItemValueCheckB :: TracePredicate
buyItemValueCheckB =
  valueAtAddress
    (walletAddress Fixtures.buyerWallet) $
    \v -> (Utils.one hasCatToken . V.flattenValue $ v) && (Utils.one hasPhotoToken . V.flattenValue $ v)
    where
      hasCatToken v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCid
      hasPhotoToken v = (v ^. _2 & V.unTokenName) == Fixtures.photoTokenIpfsCid
