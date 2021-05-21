-- | Datatypes for NFT state machine.
module Mlabs.Nft.Logic.Types(
    Nft(..)
  , toNftToken
  , Act(..)
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
  { nft'id     :: TokenName       -- ^ token name, unique identifier for NFT
  , nft'data   :: ByteString      -- ^ data (media, audio, photo, etc)
  , nft'share  :: Rational        -- ^ share for the author on each sell
  , nft'author :: UserId          -- ^ author
  , nft'owner  :: UserId          -- ^ current owner
  , nft'price  :: Maybe Integer   -- ^ price in ada, if it's nothing then nobody can buy
  }
  deriving (Show, Generic)

{-# INLINABLE toNftToken #-}
toNftToken :: ByteString -> TokenName
toNftToken = tokenName . sha2_256

-- | Actions with NFTs
data Act
  = Buy
    { act'userId   :: UserId
    , act'price    :: Integer
    , act'newPrice :: Maybe Integer
    }
  -- ^ Buy NFT and set new price
  | SetPrice
    { act'userId   :: UserId
    , act'newPrice :: Maybe Integer
    }
  -- ^ Set new price for NFT
  deriving stock (Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

--------------------------------------------------------------------------
-- boiler plate instances

PlutusTx.unstableMakeIsData ''Nft

