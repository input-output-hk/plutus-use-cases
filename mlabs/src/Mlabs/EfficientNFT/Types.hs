{-# LANGUAGE DerivingVia #-}

module Mlabs.EfficientNFT.Types (
  GenericContract,
  UserContract,
  Content (..),
  MintParams (..),
  NftId (..),
  NftCollection (..),
  NftData (..),
  SetPriceParams (..),
  ChangeOwnerParams (..),
  MintAct (..),
  ContentHash,
  Hashable (..),
  LockAct (..),
  LockDatum (..),
) where

import PlutusTx qualified
import PlutusTx.Prelude
import Prelude qualified as Hask

import Data.Aeson (FromJSON, ToJSON)
import Data.Monoid (Last)
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger (PaymentPubKeyHash (PaymentPubKeyHash), Slot, ValidatorHash (ValidatorHash))
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
  { nftId'collectionNftTn :: TokenName
  , nftId'price :: Natural
  , nftId'owner :: PaymentPubKeyHash
  }
  deriving stock (Hask.Show, Generic, Hask.Eq, Hask.Ord)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''NftId

data NftCollection = NftCollection
  { nftCollection'collectionNftCs :: CurrencySymbol
  , nftCollection'lockingScript :: ValidatorHash
  , nftCollection'author :: PaymentPubKeyHash
  , nftCollection'authorShare :: Natural
  , nftCollection'marketplaceScript :: ValidatorHash
  , nftCollection'marketplaceShare :: Natural
  }
  deriving stock (Hask.Show, Generic, Hask.Eq, Hask.Ord)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''NftCollection

data NftData = NftData
  { nftData'nftCollection :: NftCollection
  , nftData'nftId :: NftId
  }
  deriving stock (Hask.Show, Generic, Hask.Eq, Hask.Ord)
  deriving anyclass (FromJSON, ToJSON)

data SetPriceParams = SetPriceParams
  { -- | Token which price is set.
    sp'nftData :: NftData
  , -- | New price, in Lovelace.
    sp'price :: Natural
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

data ChangeOwnerParams = ChangeOwnerParams
  { -- | Token which owner is set.
    cp'nftData :: NftData
  , -- | New Owner
    cp'owner :: PaymentPubKeyHash
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

type GenericContract a = forall w s. Contract w s Text a
type UserContract a = forall s. Contract (Last NftData) s Text a

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
        [ hash $ nftId'price nft
        , hash $ nftId'owner nft
        , hash $ nftId'collectionNftTn nft
        ]

data LockAct
  = Unstake PaymentPubKeyHash Natural
  | Restake PaymentPubKeyHash Natural
  deriving stock (Hask.Show)

PlutusTx.unstableMakeIsData ''LockAct

data LockDatum = LockDatum
  { ld'sgNft :: CurrencySymbol
  , ld'entered :: Slot
  , ld'underlyingTn :: TokenName
  }
  deriving stock (Hask.Show)

instance Eq LockDatum where
  {-# INLINEABLE (==) #-}
  LockDatum a b c == LockDatum a' b' c' = a == a' && b == b' && c == c'

PlutusTx.unstableMakeIsData ''LockDatum
