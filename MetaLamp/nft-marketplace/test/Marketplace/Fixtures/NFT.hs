{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Marketplace.Fixtures.NFT where

import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Marketplace
import           PlutusTx.Builtins                            (sha2_256)
import           PlutusTx.Prelude                             (BuiltinByteString)

cids :: [Marketplace.IpfsCid]
cids = [catTokenIpfsCid, photoTokenIpfsCid]

bundleId :: Marketplace.BundleId
bundleId = Marketplace.calcBundleIdHash cids

bundleInfo :: Marketplace.BundleInfo
bundleInfo = Marketplace.BundleInfo
    { biName        = bundleName
    , biDescription = bundleDescription
    , biCategory    = bundleCategory
    }

bundleName :: BuiltinByteString
bundleName        = "Picture gallery"

bundleDescription :: BuiltinByteString
bundleDescription = "Collection of visual media"

bundleCategory :: Marketplace.Category
bundleCategory = ["User","Stan"]

catTokenIpfsCid :: Marketplace.IpfsCid
catTokenIpfsCid = "QmPeoJnaDttpFrSySYBY3reRFCzL3qv4Uiqz376EBv9W16"

catTokenIpfsCidHash :: Marketplace.IpfsCidHash
catTokenIpfsCidHash = sha2_256 catTokenIpfsCid

catTokenName :: BuiltinByteString
catTokenName = "Cat token"

catTokenDescription :: BuiltinByteString
catTokenDescription = "A picture of a cat on a pogo stick"

catTokenCategory :: Marketplace.Category
catTokenCategory = ["GIFs"]

hasCatTokenRecord :: Marketplace.NftInfo -> Bool
hasCatTokenRecord Marketplace.NftInfo {..} =
  niCategory == catTokenCategory &&
  niName == catTokenName &&
  niDescription == catTokenDescription

photoTokenIpfsCid :: Marketplace.IpfsCid
photoTokenIpfsCid = "QmeSFBsEZ7XtK7yv5CQ79tqFnH9V2jhFhSSq1LV5W3kuiB"

photoTokenIpfsCidHash :: Marketplace.IpfsCidHash
photoTokenIpfsCidHash = sha2_256 photoTokenIpfsCid

photoTokenName :: BuiltinByteString
photoTokenName = "Photo token"

photoTokenDescription :: BuiltinByteString
photoTokenDescription = "A picture of a sunset"

photoTokenCategory :: Marketplace.Category
photoTokenCategory = ["Photos"]

hasPhotoTokenRecord :: Marketplace.NftInfo -> Bool
hasPhotoTokenRecord Marketplace.NftInfo {..} =
  niCategory == photoTokenCategory &&
  niName == photoTokenName &&
  niDescription == photoTokenDescription

oneAdaInLovelace :: Integer
oneAdaInLovelace = 1000000
