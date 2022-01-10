module Mlabs.EfficientNFT.Types (
  GenericContract,
  UserContract,
  Content (..),
  MintParams (..),
  NftId(..),
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
import Plutus.V1.Ledger.Value (AssetClass)
import PlutusTx.Natural (Natural)
import Schema (ToSchema (toSchema))
import Plutus.V1.Ledger.Api (MintingPolicy)

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
    mp'share :: Rational
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
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)


type GenericContract a = forall w s. Contract w s Text a
type UserContract a = forall s. Contract (Last NftId) s Text a
