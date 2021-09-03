{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Marketplace.Spec.Bundles
  ( tests, bundleTrace
  ) where

import           Control.Lens                                 ((^.), (^?))
import           Control.Monad                                (void)
import           Data.Maybe                                   (isNothing)
import           Data.Text                                    (Text)
import           Data.Void                                    (Void)
import qualified Marketplace.Fixtures                         as Fixtures
import qualified Marketplace.Spec.Start                       as Start
import           Plutus.Abstract.ContractResponse             (ContractResponse)
import           Plutus.Contract.Test
import qualified Plutus.Contracts.NftMarketplace.Endpoints    as Marketplace
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Marketplace
import qualified Plutus.Trace                                 as Trace
import qualified PlutusTx.AssocMap                            as AssocMap
import           Test.Tasty
import qualified Utils

tests :: TestTree
tests =
  testGroup
    "Bundles"
    [
      checkPredicateOptions
        Fixtures.options
        "Should create a bundle for two NFTs transforming Marketplace store"
        bundleDatumsCheck
        (void bundleTrace),
      checkPredicateOptions
        Fixtures.options
        "Should not create a bundle if NFTs are not minted"
        errorCheck
        bundleErrorTrace,
      checkPredicateOptions
        Fixtures.options
        "Should unbundle transforming Marketplace store"
        unbundleDatumsCheck
        unbundleTrace,
      checkPredicateOptions
        Fixtures.options
        "Should not unbundle if bundle does not exist"
        errorCheck
        unbundleErrorTrace
    ]

bundleUpParams ::        Marketplace.BundleUpParams
bundleUpParams =  Marketplace.BundleUpParams {
          Marketplace.bupIpfsCids    = Fixtures.cids,
          Marketplace.bupName        =  Fixtures.bundleName,
          Marketplace.bupDescription =  Fixtures.bundleDescription,
          Marketplace.bupCategory    = Fixtures.bundleCategory
        }

bundleTrace :: Trace.EmulatorTrace (Trace.ContractHandle (ContractResponse Text Marketplace.UserContractState) Marketplace.MarketplaceUserSchema Void)
bundleTrace = do
  _ <- Start.startTrace
  h <- Trace.activateContractWallet Fixtures.userWallet $ Marketplace.userEndpoints Fixtures.marketplace

  _ <- Trace.callEndpoint @"createNft" h
          Marketplace.CreateNftParams {
                        Marketplace.cnpIpfsCid        = Fixtures.catTokenIpfsCid,
                        Marketplace.cnpNftName        = Fixtures.catTokenName,
                        Marketplace.cnpNftDescription = Fixtures.catTokenDescription,
                        Marketplace.cnpNftCategory = Fixtures.catTokenCategory,
                        Marketplace.cnpRevealIssuer   = False
                    }
  _ <- Trace.waitNSlots 50
  _ <- Trace.callEndpoint @"createNft" h
          Marketplace.CreateNftParams {
                        Marketplace.cnpIpfsCid        = Fixtures.photoTokenIpfsCid,
                        Marketplace.cnpNftName        = Fixtures.photoTokenName,
                        Marketplace.cnpNftDescription = Fixtures.photoTokenDescription,
                        Marketplace.cnpNftCategory = Fixtures.photoTokenCategory,
                        Marketplace.cnpRevealIssuer   = False
                    }
  _ <- Trace.waitNSlots 50
  _ <- Trace.callEndpoint @"bundleUp" h bundleUpParams

  _ <- Trace.waitNSlots 50
  pure h

bundleErrorTrace :: Trace.EmulatorTrace ()
bundleErrorTrace = do
  _ <- Start.startTrace
  h <- Trace.activateContractWallet Fixtures.userWallet $ Marketplace.userEndpoints Fixtures.marketplace

  _ <- Trace.callEndpoint @"bundleUp" h bundleUpParams

  _ <- Trace.waitNSlots 50
  pure ()

bundleDatumsCheck :: TracePredicate
bundleDatumsCheck =
  dataAtAddress
    Fixtures.marketplaceAddress
    (Utils.checkOneDatum (containsBundle . Marketplace.mdBundles))
    where
      containsBundle = maybe False checkBundle .
                       AssocMap.lookup Fixtures.bundleId
      checkBundle b = maybe False containsNfts (b ^? Marketplace._nbTokens . Marketplace._NoLot) &&
                      b ^. Marketplace._nbRecord == Fixtures.bundleInfo
      containsNfts b = maybe False Fixtures.hasCatTokenRecord
                       (AssocMap.lookup Fixtures.catTokenIpfsCidHash b) &&
                       maybe False Fixtures.hasPhotoTokenRecord
                       (AssocMap.lookup Fixtures.photoTokenIpfsCidHash b)

errorCheck :: TracePredicate
errorCheck = Utils.assertCrError (Marketplace.userEndpoints Fixtures.marketplace) (Trace.walletInstanceTag Fixtures.userWallet)

unbundleTrace :: Trace.EmulatorTrace ()
unbundleTrace = do
  h <- bundleTrace

  _ <- Trace.callEndpoint @"unbundle" h $ Marketplace.UnbundleParams Fixtures.cids

  _ <- Trace.waitNSlots 50
  pure ()

unbundleErrorTrace :: Trace.EmulatorTrace ()
unbundleErrorTrace = do
  _ <- Start.startTrace
  h <- Trace.activateContractWallet Fixtures.userWallet $ Marketplace.userEndpoints Fixtures.marketplace

  _ <- Trace.callEndpoint @"unbundle" h $ Marketplace.UnbundleParams Fixtures.cids

  _ <- Trace.waitNSlots 50
  pure ()

unbundleDatumsCheck :: TracePredicate
unbundleDatumsCheck =
  dataAtAddress
    Fixtures.marketplaceAddress $ Utils.checkOneDatum $
    \mp -> (containsNoBundle . Marketplace.mdBundles $ mp) && (containsNfts . Marketplace.mdSingletons $ mp)
    where
      containsNoBundle = isNothing . AssocMap.lookup Fixtures.bundleId
      containsNfts store = maybe False (Fixtures.hasCatTokenRecord . Marketplace.nftRecord)
                       (AssocMap.lookup Fixtures.catTokenIpfsCidHash store) &&
                       maybe False (Fixtures.hasPhotoTokenRecord . Marketplace.nftRecord)
                       (AssocMap.lookup Fixtures.photoTokenIpfsCidHash store)
