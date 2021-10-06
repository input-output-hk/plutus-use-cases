{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFT.Types (
  UserId (..),
  QueryResponse (..),
  NftId (..),
  BuyRequestUser (..),
  MintParams (..),
  SetPriceParams (..),
  Content (..),
  Title (..),
) where

import PlutusTx.Prelude
import Prelude qualified as Hask

import Data.Aeson (FromJSON, ToJSON)
import Data.Monoid (Last)
import GHC.Generics (Generic)

import Ledger (PubKeyHash, TokenName, TxOutRef)
import PlutusTx qualified
import Schema (ToSchema)

-- ON-CHAIN TYPES --
newtype Content = Content {getContent :: BuiltinByteString}
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''Content
PlutusTx.makeLift ''Content

instance Eq Content where
  {-# INLINEABLE (==) #-}
  (Content c1) == (Content c2) = c1 == c2

newtype Title = Title {getTitle :: BuiltinByteString}
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''Title
PlutusTx.makeLift ''Title

instance Eq Title where
  {-# INLINEABLE (==) #-}
  (Title t1) == (Title t2) = t1 == t2

newtype UserId = UserId {getUserId :: PubKeyHash}
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''UserId
PlutusTx.makeLift ''UserId

instance Eq UserId where
  {-# INLINEABLE (==) #-}
  (UserId u1) == (UserId u2) = u1 == u2

{- | Unique identifier of NFT.
 The NftId contains a human readable title, the hashed information of the
 content and the utxo ref included when minting the token.
-}
data NftId = NftId
  { -- | Content Title.
    nftId'title :: Title
  , -- | token name is identified by content of the NFT (it's hash of it)
    nftId'token :: TokenName
  , -- | TxOutRef which was used to mint current NFT
    nftId'outRef :: TxOutRef
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

PlutusTx.unstableMakeIsData ''NftId
PlutusTx.makeLift ''NftId

instance Eq NftId where
  {-# INLINEABLE (==) #-}
  (NftId title1 token1 outRef1) == (NftId title2 token2 outRef2) =
    title1 == title2 && token1 == token2 && outRef1 == outRef2

{- | Type representing the data that gets hashed when the token is minted. The
 tile and author are included for proof of authenticity in the case the same
 artwork is hashed more than once.
-}
data NftContent = NftContent
  { -- | Content Title.
    ct'title :: Title
  , -- | data (NftContent, audio, photo, etc)
    ct'data :: Content
  , -- | author
    ct'author :: UserId
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''NftContent
PlutusTx.makeLift ''NftContent

instance Eq NftContent where
  {-# INLINEABLE (==) #-}
  (NftContent title1 data1 author1) == (NftContent title2 data2 author2) =
    title1 == title2 && data1 == data2 && author1 == author2

-- ENDPOINTS PARAMETERS --

-- | Parameters that need to be submitted when minting a new NFT.
data MintParams = MintParams
  { -- | Content to be minted.
    mp'content :: Content
  , -- | Title of content.
    mp'title :: Title
  , -- | Shares retained by author.
    mp'share :: Rational
  , -- | Listing price of the NFT.
    mp'price :: Maybe Integer
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

PlutusTx.makeLift ''MintParams
PlutusTx.unstableMakeIsData ''MintParams

instance Eq MintParams where
  {-# INLINEABLE (==) #-}
  (MintParams content1 title1 share1 price1) == (MintParams content2 title2 share2 price2) =
    content1 == content2 && title1 == title2 && share1 == share2 && price1 == price2

data SetPriceParams = SetPriceParams
  { sp'nftId :: NftId
  , sp'price :: Maybe Integer -- todo maybe Natural? are they available here?
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

instance Eq SetPriceParams where
  {-# INLINEABLE (==) #-}
  (SetPriceParams nftId1 price1) == (SetPriceParams nftId2 price2) =
    nftId1 == nftId2 && price1 == price2

data BuyRequestUser = BuyRequestUser
  { -- | nftId to Buy
    ur'nftId :: NftId
  , -- | price to buy
    ur'price :: Integer
  , -- | new price for NFT (Nothing locks NFT)
    ur'newPrice :: Maybe Integer
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.makeLift ''BuyRequestUser
PlutusTx.unstableMakeIsData ''BuyRequestUser

instance Eq BuyRequestUser where
  {-# INLINEABLE (==) #-}
  (BuyRequestUser nftId1 price1 newPrice1) == (BuyRequestUser nftId2 price2 newPrice2) =
    nftId1 == nftId2 && price1 == price2 && newPrice1 == newPrice2

-- | A datatype used by the QueryContract to return a response
data QueryResponse
  = QueryCurrentOwner (Last UserId)
  | QueryCurrentPrice (Last Integer)
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)
