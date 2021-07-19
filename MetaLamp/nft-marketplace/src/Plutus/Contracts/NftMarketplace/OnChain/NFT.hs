module Plutus.Contracts.NftMarketplace.OnChain.NFT where

import qualified Data.Text as T

data NFT = NFT
  { nftId :: T.Text
  , nftName :: T.Text
  , nftDescription :: T.Text
  , nftIssuer :: Maybe PubKey
  , nftIpfsCid :: T.Text
  }

-- Marketplace model:
-- 1. Store only files from IPFS network
-- 2. NFT ID is a hash of IpfsCid
-- 3. Store "data NFT" above in scripts datum as (Map nftId "data NFT")
-- 4. TokenName of a NFT is the nftId
-- 5. Only user with NFT (inside his wallet) identified by the nftId has access to view "data NFT"
-- 6. Nobody can modify "data NFT"
-- 7. Nobody can mint another NFT with same nftId
-- 8. Nobody can burn NFT (?)
-- TODO do we store NFT metadata from Cardano in IPFS also?

-- TODO wrap "data NFT" into (isOnSale, "data NFT")
-- to give access to view "data NFT" which are on sale for other users
-- (only user with NFT inside wallet could change isOnSale)
