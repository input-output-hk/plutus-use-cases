-- | Datatypes for NFT state machine.
module Mlabs.Nft.Logic.Types where

import Data.Aeson (FromJSON, ToJSON)

import qualified Prelude as Hask
import qualified PlutusTx as PlutusTx
import PlutusTx.Prelude
import Plutus.V1.Ledger.Value (AssetClass(..), TokenName(..), CurrencySymbol(..))
import GHC.Generics

import Mlabs.Lending.Logic.Types (UserId(..))

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

-- | Acts with NFTs
data Act
  = Buy
    { act'userId   :: UserId
    , act'price    :: Integer
    , act'newPrice :: Maybe Integer
    }
  | SetPrice
    { act'userId   :: UserId
    , act'newPrice :: Maybe Integer
    }
  deriving stock (Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

--------------------------------------------------------------------------
-- boiler plate instances

PlutusTx.unstableMakeIsData ''Nft

