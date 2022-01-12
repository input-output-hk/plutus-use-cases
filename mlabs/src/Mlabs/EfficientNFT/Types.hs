module Mlabs.EfficientNFT.Types (
  GenericContract,
  UserContract,
  Content (..),
  MintParams (..),
  NftId (..),
  SetPriceParams (..),
  ChangeOwnerParams (..),
) where

import PlutusTx qualified
import PlutusTx.Prelude
import Prelude qualified as Hask

import Data.Aeson (FromJSON, ToJSON)
import Data.Monoid (Last)
import Data.Text (Text)
import GHC.Generics (Generic)
import Mlabs.NFT.PAB.MarketplaceContract (MarketplaceContracts (UserContract))
import Plutus.Contract (Contract)
import Plutus.V1.Ledger.Api (MintingPolicy, PubKeyHash)
import Plutus.V1.Ledger.Value (AssetClass)
import PlutusTx.Natural (Natural)
import Schema (ToSchema (toSchema))

newtype Content = Content {getContent :: BuiltinByteString}
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''Content
PlutusTx.makeLift ''Content

-- | Parameters that need to be submitted when minting a new NFT.
data MintParams = MintParams
  { -- | Content to be minted.
    mp'content :: Content
  , -- | Shares retained by author.
    mp'share :: Natural
  , -- | Listing price of the NFT, in Lovelace.
    mp'price :: Natural
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

PlutusTx.unstableMakeIsData ''MintParams
PlutusTx.makeLift ''MintParams

data NftId = NftId
  { nftId'assetClass :: AssetClass
  , nftId'policy :: MintingPolicy
  , nftId'price :: Natural
  , nftId'owner :: PubKeyHash
  , nftId'author :: PubKeyHash
  , nftId'authorShare :: Natural
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

data SetPriceParams = SetPriceParams
  { -- | Token which price is set.
    sp'nftId :: NftId
  , -- | New price, in Lovelace.
    sp'price :: Natural
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

data ChangeOwnerParams = ChangeOwnerParams
  { -- | Token which owner is set.
    cp'nftId :: NftId
  , -- | New Owner
    cp'owner :: PubKeyHash
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

type GenericContract a = forall w s. Contract w s Text a
type UserContract a = forall s. Contract (Last NftId) s Text a
