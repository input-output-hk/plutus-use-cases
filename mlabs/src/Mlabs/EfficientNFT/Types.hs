{-# LANGUAGE DerivingVia #-}

module Mlabs.EfficientNFT.Types (
  GenericContract,
  UserContract,
  Content (..),
  MintParams (..),
  NftId (..),
  SetPriceParams (..),
  ChangeOwnerParams (..),
  MintAct (..),
  ContentHash,
  Hashable (..),
) where

import PlutusTx qualified
import PlutusTx.Prelude
import Prelude qualified as Hask

import Data.Aeson (FromJSON, ToJSON)
import Data.Monoid (Last)
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger (PaymentPubKeyHash (PaymentPubKeyHash), ValidatorHash (ValidatorHash))
import Plutus.Contract (Contract)
import Plutus.V1.Ledger.Crypto (PubKeyHash (PubKeyHash))
import Plutus.V1.Ledger.Value (AssetClass (AssetClass), CurrencySymbol (CurrencySymbol), TokenName (TokenName))
import PlutusTx.Natural (Natural)
import Schema (ToSchema)

newtype Content = Content {getContent :: BuiltinByteString}
  deriving stock (Hask.Show, Generic, Hask.Eq, Hask.Ord)
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
  { nftId'content :: Content
  , nftId'collectionNft :: AssetClass
  , nftId'price :: Natural
  , nftId'owner :: PaymentPubKeyHash
  , nftId'author :: PaymentPubKeyHash
  , nftId'authorShare :: Natural
  , nftId'marketplaceValHash :: ValidatorHash
  , nftId'marketplaceShare :: Natural
  }
  deriving stock (Hask.Show, Generic, Hask.Eq, Hask.Ord)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''NftId

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
    cp'owner :: PaymentPubKeyHash
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

type GenericContract a = forall w s. Contract w s Text a
type UserContract a = forall s. Contract (Last NftId) s Text a

data MintAct
  = MintToken NftId
  | ChangePrice NftId Natural
  | ChangeOwner NftId PaymentPubKeyHash
  | BurnToken NftId
  deriving stock (Hask.Show)

PlutusTx.unstableMakeIsData ''MintAct

type ContentHash = BuiltinByteString

class Hashable a where
  hash :: a -> BuiltinByteString

instance Hashable BuiltinByteString where
  {-# INLINEABLE hash #-}
  hash = sha2_256

instance Hashable Natural where
  {-# INLINEABLE hash #-}
  hash = sha2_256 . toBin . fromEnum
    where
      {-# INLINEABLE toBin #-}
      toBin :: Integer -> BuiltinByteString
      toBin n = toBin' n mempty
        where
          toBin' n' rest
            | n' < 256 = consByteString n' rest
            | otherwise = toBin' (n' `divide` 256) (consByteString (n' `modulo` 256) rest)

instance (Hashable a, Hashable b) => Hashable (a, b) where
  hash (a, b) = hash (hash a <> hash b)

deriving via BuiltinByteString instance Hashable Content
deriving via BuiltinByteString instance Hashable ValidatorHash
deriving via BuiltinByteString instance Hashable PaymentPubKeyHash
deriving via BuiltinByteString instance Hashable TokenName
deriving via BuiltinByteString instance Hashable CurrencySymbol
deriving via (CurrencySymbol, TokenName) instance Hashable AssetClass

instance Hashable NftId where
  {-# INLINEABLE hash #-}
  hash nft =
    hash $
      mconcat
        [ hash $ nftId'content nft
        , hash $ nftId'collectionNft nft
        , hash $ nftId'price nft
        , hash $ nftId'owner nft
        , hash $ nftId'author nft
        , hash $ nftId'authorShare nft
        , hash $ nftId'marketplaceValHash nft
        , hash $ nftId'marketplaceShare nft
        ]
