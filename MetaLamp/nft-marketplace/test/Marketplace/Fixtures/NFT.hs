{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Marketplace.Fixtures.NFT where

-- import           Data.Text                                    (Text)
-- import qualified Plutus.Contracts.NftMarketplace.Endpoints    as Marketplace
-- import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Marketplace
-- import           PlutusTx.Builtins                            (sha2_256)
-- import           PlutusTx.Prelude                             (BuiltinByteString)

-- cids :: [Text]
-- cids = [catTokenIpfsCid, photoTokenIpfsCid]

-- catTokenIpfsCid :: Text
-- catTokenIpfsCid = "QmPeoJnaDttpFrSySYBY3reRFCzL3qv4Uiqz376EBv9W16"

-- catTokenIpfsCidBs :: BuiltinByteString
-- catTokenIpfsCidBs = "QmPeoJnaDttpFrSySYBY3reRFCzL3qv4Uiqz376EBv9W16"

-- catTokenIpfsCidHash :: Marketplace.IpfsCidHash
-- catTokenIpfsCidHash = sha2_256 $ Marketplace.deserializeByteString catTokenIpfsCid

-- catTokenName :: Text
-- catTokenName = "Cat token"

-- catTokenDescription :: Text
-- catTokenDescription = "A picture of a cat on a pogo stick"

-- catTokenCategory :: [Text]
-- catTokenCategory = ["GIFs"]

-- hasCatTokenRecord :: Marketplace.NftInfo -> Bool
-- hasCatTokenRecord Marketplace.NftInfo {..} =
--   niCategory == (Marketplace.deserializePlutusBuiltinBS <$> catTokenCategory) &&
--   niName == (Marketplace.deserializePlutusBuiltinBS catTokenName) &&
--   niDescription == (Marketplace.deserializePlutusBuiltinBS catTokenDescription)

-- photoTokenIpfsCid :: Text
-- photoTokenIpfsCid = "QmeSFBsEZ7XtK7yv5CQ79tqFnH9V2jhFhSSq1LV5W3kuiB"

-- photoTokenIpfsCidBs :: BuiltinByteString
-- photoTokenIpfsCidBs = "QmeSFBsEZ7XtK7yv5CQ79tqFnH9V2jhFhSSq1LV5W3kuiB"

-- photoTokenIpfsCidHash :: Marketplace.IpfsCidHash
-- photoTokenIpfsCidHash = sha2_256 $ Marketplace.deserializeByteString photoTokenIpfsCid

-- photoTokenName :: Text
-- photoTokenName = "Photo token"

-- photoTokenDescription :: Text
-- photoTokenDescription = "A picture of a sunset"

-- photoTokenCategory :: [Text]
-- photoTokenCategory = ["Photos"]

-- hasPhotoTokenRecord :: Marketplace.NftInfo -> Bool
-- hasPhotoTokenRecord Marketplace.NftInfo {..} =
--   niCategory == (Marketplace.deserializePlutusBuiltinBS <$> photoTokenCategory) &&
--   niName == (Marketplace.deserializePlutusBuiltinBS photoTokenName) &&
--   niDescription == (Marketplace.deserializePlutusBuiltinBS photoTokenDescription)

-- oneAdaInLovelace :: Integer
-- oneAdaInLovelace = 1000000
