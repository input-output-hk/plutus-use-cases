{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fobject-code #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

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
import Playground.Contract (TxOutRef, ToSchema)
import Plutus.V1.Ledger.TxId

import Mlabs.Emulator.Types (UserId(..))
import Mlabs.Data.Ray (Ray)

-- | Data for NFTs
data Nft = Nft
  { nft'id     :: NftId           -- ^ token name, unique identifier for NFT
  , nft'data   :: ByteString      -- ^ data (media, audio, photo, etc)
  , nft'share  :: Ray             -- ^ share for the author on each sell
  , nft'author :: UserId          -- ^ author
  , nft'owner  :: UserId          -- ^ current owner
  , nft'price  :: Maybe Integer   -- ^ price in ada, if it's nothing then nobody can buy
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Unique identifier of NFT.
data NftId = NftId
  { nftId'token  :: TokenName     -- ^ token name is identified by content of the NFT (it's hash of it)
  , nftId'outRef :: TxOutRef      -- ^ TxOutRef that is used for minting of NFT,
                                  -- with it we can guarantee unqiqueness of NFT
  }
  deriving stock (Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

deriving newtype instance ToSchema TxId
deriving instance ToSchema TxOutRef

instance Eq NftId where
  {-# INLINABLE (==) #-}
  (==) (NftId tok1 oref1) (NftId tok2 oref2) =
    tok1 == tok2 && oref1 == oref2

{-# INLINABLE initNft #-}
-- | Initialise NFT
initNft :: TxOutRef -> UserId -> ByteString -> Ray -> Maybe Integer -> Nft
initNft nftInRef author content share mPrice = Nft
  { nft'id     = toNftId nftInRef content
  , nft'data   = content
  , nft'share  = share
  , nft'author = author
  , nft'owner  = author
  , nft'price  = mPrice
  }

{-# INLINABLE toNftId #-}
-- | Calculate NFT identifier from it's content (data).
toNftId :: TxOutRef -> ByteString -> NftId
toNftId oref content = NftId (tokenName $ sha2_256 content) oref

-- | Actions with NFTs with UserId.
data Act = UserAct UserId UserAct
  deriving stock (Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Actions with NFTs
data UserAct
  = BuyAct
    { act'price    :: Integer       -- ^ price to buy
    , act'newPrice :: Maybe Integer -- ^ new price for NFT (Nothing locks NFT)
    }
  -- ^ Buy NFT and set new price
  | SetPriceAct
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
PlutusTx.unstableMakeIsData ''NftId
PlutusTx.makeLift ''NftId
