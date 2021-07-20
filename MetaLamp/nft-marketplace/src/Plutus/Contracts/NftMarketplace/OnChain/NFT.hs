module Plutus.Contracts.NftMarketplace.OnChain.NFT where

import qualified Data.Text        as T
import           Ledger
import           Plutus.Contract
import           PlutusTx.Prelude hiding (Semigroup (..))

-- newtype AssetClass = AssetClass { unAssetClass :: (CurrencySymbol, TokenName) }
data NFT =
  NFT
    { nftId          :: CurrencySymbol
    , nftName        :: T.Text
    , nftDescription :: T.Text
    , nftIssuer      :: Maybe PubKeyHash -- ???? what if issuer is the owner, wouldn't that violate privacy TODO rm field?
    }

-- Marketplace model:
-- . Store only files from IPFS network. Get the IpfsCid
-- . NFT token should be minted first in a separate transaction so that state token won't be linked to IpfsCid
-- . Minting policy is parametrized by IpfsCid
-- . TokenName of a NFT is the IpfsCid
-- . NFT ID is the CurrencySymbol
-- . Store "data NFT" above in scripts datum as (Map nftId "data NFT")
-- . Only user with NFT (inside his wallet) identified by the nftId has access to view "data NFT"
-- . Nobody can modify "data NFT"
-- . Nobody can mint another NFT with same nftId
-- . Nobody can burn NFT (?)
-- TODO do we store NFT metadata from Cardano in IPFS also?

-- TODO wrap "data NFT" into (isOnSale, "data NFT")
-- to give access to view "data NFT" which are on sale for other users
-- (only user with NFT inside wallet could change isOnSale)
