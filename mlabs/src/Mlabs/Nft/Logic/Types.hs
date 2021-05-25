{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fobject-code #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
-- | Datatypes for NFT state machine.
module Mlabs.Nft.Logic.Types(
    Nft(..)
  , NftId(..)
  , initNft
  , toNftId
  , Act(..)
  , UserAct(..)
) where

import Data.Aeson (FromJSON, ToJSON)

import qualified Prelude as Hask
import qualified PlutusTx as PlutusTx
import PlutusTx.Prelude
import Plutus.V1.Ledger.Value (TokenName(..), tokenName)
import GHC.Generics

import Mlabs.Emulator.Types (UserId(..))

-- | Data for NFTs
data Nft = Nft
  { nft'id     :: NftId           -- ^ token name, unique identifier for NFT
  , nft'data   :: ByteString      -- ^ data (media, audio, photo, etc)
  , nft'share  :: Rational        -- ^ share for the author on each sell
  , nft'author :: UserId          -- ^ author
  , nft'owner  :: UserId          -- ^ current owner
  , nft'price  :: Maybe Integer   -- ^ price in ada, if it's nothing then nobody can buy
  }
  deriving (Show, Generic)

-- | Unique identifier of NFT.
newtype NftId = NftId TokenName
  deriving newtype (Show, Eq, PlutusTx.IsData)

{-# INLINABLE initNft #-}
initNft :: UserId -> ByteString -> Rational -> Maybe Integer -> Nft
initNft author content share mPrice = Nft
  { nft'id     = toNftId content
  , nft'data   = content
  , nft'share  = share
  , nft'author = author
  , nft'owner  = author
  , nft'price  = mPrice
  }

{-# INLINABLE toNftId #-}
-- | Calculate NFT identifier from it's content (data).
toNftId :: ByteString -> NftId
toNftId = NftId . tokenName . sha2_256

data Act = UserAct UserId UserAct
  deriving stock (Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Actions with NFTs
data UserAct
  = Buy
    { act'price    :: Integer       -- ^ price to buy
    , act'newPrice :: Maybe Integer -- ^ new price for NFT (Nothing locks NFT)
    }
  -- ^ Buy NFT and set new price
  | SetPrice
    { act'newPrice :: Maybe Integer -- ^ new price for NFT (Nothing locks NFT)
    }
  -- ^ Set new price for NFT
  deriving stock (Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

--------------------------------------------------------------------------
-- boiler plate instances

PlutusTx.unstableMakeIsData ''Nft
PlutusTx.unstableMakeIsData ''UserAct
PlutusTx.unstableMakeIsData ''Act
PlutusTx.makeLift ''NftId
