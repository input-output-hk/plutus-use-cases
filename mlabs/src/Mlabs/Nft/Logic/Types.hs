{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fobject-code #-}

-- | Datatypes for NFT state machine.
module Mlabs.Nft.Logic.Types (
  Nft (..),
  NftId (..),
  initNft,
  toNftId,
  Act (..),
  UserAct (..),
) where

import PlutusTx.Prelude

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Playground.Contract (ToSchema, TxOutRef)
import Plutus.V1.Ledger.TxId (TxId (TxId))
import Plutus.V1.Ledger.Value (TokenName (..), tokenName)
import PlutusTx qualified
import Prelude qualified as Hask (Eq, Show)

import Mlabs.Emulator.Types (UserId (..))

-- | Data for NFTs
data Nft = Nft
  { -- | token name, unique identifier for NFT
    nft'id :: NftId
  , -- | data (media, audio, photo, etc)
    nft'data :: BuiltinByteString
  , -- | share for the author on each sell
    nft'share :: Rational
  , -- | author
    nft'author :: UserId
  , -- | current owner
    nft'owner :: UserId
  , -- | price in ada, if it's nothing then nobody can buy
    nft'price :: Maybe Integer
  }
  deriving stock (Hask.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Unique identifier of NFT.
data NftId = NftId
  { -- | token name is identified by content of the NFT (it's hash of it)
    nftId'token :: TokenName
  , -- | TxOutRef that is used for minting of NFT,
    -- with it we can guarantee uniqueness of NFT
    nftId'outRef :: TxOutRef
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- deriving newtype instance ToSchema TxId
deriving instance ToSchema TxOutRef

instance Eq NftId where
  {-# INLINEABLE (==) #-}
  (==) (NftId tok1 oref1) (NftId tok2 oref2) =
    tok1 == tok2 && oref1 == oref2

{-# INLINEABLE initNft #-}

-- | Initialise NFT
initNft :: TxOutRef -> UserId -> BuiltinByteString -> Rational -> Maybe Integer -> Nft
initNft nftInRef author content share mPrice =
  Nft
    { nft'id = toNftId nftInRef content
    , nft'data = content
    , nft'share = share
    , nft'author = author
    , nft'owner = author
    , nft'price = mPrice
    }

{-# INLINEABLE toNftId #-}

-- | Calculate NFT identifier from it's content (data).
toNftId :: TxOutRef -> BuiltinByteString -> NftId
toNftId oref content = NftId (TokenName $ sha2_256 content) oref

-- | Actions with NFTs with UserId.
data Act = UserAct UserId UserAct
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Actions with NFTs
data UserAct
  = -- | Buy NFT and set new price
    BuyAct
      { -- | price to buy
        act'price :: Integer
      , -- | new price for NFT (Nothing locks NFT)
        act'newPrice :: Maybe Integer
      }
  | -- | Set new price for NFT
    SetPriceAct
      { -- | new price for NFT (Nothing locks NFT)
        act'newPrice :: Maybe Integer
      }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

--------------------------------------------------------------------------
-- boiler plate instances

PlutusTx.unstableMakeIsData ''Nft
PlutusTx.unstableMakeIsData ''UserAct
PlutusTx.unstableMakeIsData ''Act
PlutusTx.unstableMakeIsData ''NftId
PlutusTx.makeLift ''NftId
