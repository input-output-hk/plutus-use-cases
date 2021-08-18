{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}

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
    ]

openSaleParams ::        Marketplace.OpenSaleParams
openSaleParams =  Marketplace.OpenSaleParams {
                    ospItemId   = Marketplace.UserNftId Fixtures.catTokenIpfsCid,
                    ospSalePrice = 44 * Fixtures.oneAdaInLovelace
                   }

completeSaleParams ::        Marketplace.CompleteSaleParams
completeSaleParams =  Marketplace.CompleteSaleParams {
                      cspItemId    = Marketplace.UserNftId Fixtures.catTokenIpfsCid
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

  _ <- Trace.callEndpoint @"closeSale" h completeSaleParams

  _ <- Trace.waitNSlots 50
  pure ()

closeSaleTrace' :: Trace.EmulatorTrace ()
closeSaleTrace' = do
  h <- CreateNft.createNftTrace

  _ <- Trace.callEndpoint @"closeSale" h completeSaleParams

  _ <- Trace.waitNSlots 50
  pure ()

buyItemTrace :: Trace.EmulatorTrace ()
buyItemTrace = do
  _ <- openSaleTrace

  h <- Trace.activateContractWallet Fixtures.buyerWallet $ Marketplace.userEndpoints Fixtures.marketplace
  _ <- Trace.callEndpoint @"buyItem" h completeSaleParams

  _ <- Trace.waitNSlots 50
  pure ()

buyItemTrace' :: Trace.EmulatorTrace ()
buyItemTrace' = do
  _ <- CreateNft.createNftTrace

  h <- Trace.activateContractWallet Fixtures.buyerWallet $ Marketplace.userEndpoints Fixtures.marketplace
  _ <- Trace.callEndpoint @"buyItem" h completeSaleParams

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
