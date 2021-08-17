{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}

module Marketplace.Spec.Bundles
  ( tests
  ) where

import           Control.Lens                                 (_2, (&), (.~),
                                                               (^.), (^?))
import qualified Control.Lens                                 as Lens
import           Control.Monad                                (void)
import qualified Data.Map                                     as Map
import           Data.Maybe                                   (isNothing)
import           Data.Text                                    (Text)
import           Ledger
import qualified Ledger.Ada                                   as Ada
import qualified Ledger.Value                                 as V
import qualified Marketplace.Fixtures                         as Fixtures
import qualified Marketplace.Spec.Start                       as Start
import           Plutus.Contract
import           Plutus.Contract.Test
import qualified Plutus.Contracts.NftMarketplace.Endpoints    as Marketplace
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Marketplace
import qualified Plutus.Trace                                 as Trace
import qualified PlutusTx.AssocMap                            as AssocMap
import           PlutusTx.Builtins                            (sha2_256)
import           PlutusTx.Prelude                             (ByteString)
import           Test.Tasty
import qualified Utils
import           Wallet.Emulator.Wallet

-- Should unbunle
-- Should not create if NFT not minted
tests :: TestTree
tests =
  testGroup
    "Bundles"
    [
      checkPredicateOptions
        Fixtures.options
        "Should create a bundle for two NFTs transforming Marketplace store"
        datumsCheck
        bundleUpTrace,
      checkPredicateOptions
        Fixtures.options
        "Should not create a bundle if NFTs are not minted"
        bundleErrorCheck
        bundleErrorTrace
    ]

bundleUpParams ::        Marketplace.BundleUpParams
bundleUpParams =  Marketplace.BundleUpParams {
          bupIpfsCids    = Fixtures.cids,
          bupName        =  Fixtures.bundleName,
          bupDescription =  Fixtures.bundleDescription,
          bupCategory    = Fixtures.bundleCategory
        }

bundleUpTrace :: Trace.EmulatorTrace ()
bundleUpTrace = do
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
  pure ()

bundleErrorTrace :: Trace.EmulatorTrace ()
bundleErrorTrace = do
  _ <- Start.startTrace
  h <- Trace.activateContractWallet Fixtures.userWallet $ Marketplace.userEndpoints Fixtures.marketplace

  _ <- Trace.callEndpoint @"bundleUp" h bundleUpParams

  _ <- Trace.waitNSlots 50
  pure ()

datumsCheck :: TracePredicate
datumsCheck =
  dataAtAddress
    Fixtures.marketplaceAddress
    (containsBundle . Marketplace.mdBundles)
    where
      containsBundle = maybe False checkBundle .
                       (AssocMap.lookup Fixtures.bundleId)
      checkBundle b = maybe False containsNfts (b ^? Marketplace._nbTokens . Marketplace._NoLot) &&
                      (b ^. Marketplace._nbRecord == Fixtures.bundleInfo)
      containsNfts b = maybe False Fixtures.hasCatTokenRecord
                       (AssocMap.lookup Fixtures.catTokenIpfsCidHash b) &&
                       maybe False Fixtures.hasPhotoTokenRecord
                       (AssocMap.lookup Fixtures.photoTokenIpfsCidHash b)

bundleErrorCheck :: TracePredicate
bundleErrorCheck = Utils.assertCrError (Marketplace.userEndpoints Fixtures.marketplace) (Trace.walletInstanceTag Fixtures.userWallet)
