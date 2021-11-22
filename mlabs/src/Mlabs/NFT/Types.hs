{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFT.Types (
  UserContract,
  UserWriter,
  AdminContract,
  UserId (..),
  QueryResponse (..),
  NftId (..),
  BuyRequestUser (..),
  MintParams (..),
  MintAct (..),
  SetPriceParams (..),
  Content (..),
  Title (..),
  DatumNft (..),
  NftAppInstance (..),
  UserAct (..),
  InformationNft (..),
  NftListNode (..),
  NftListHead (..),
  NftAppSymbol (..),
  Pointer (..),
  nftTokenName,
  getAppInstance,
  instanceCurrency,
  getDatumPointer,
  getDatumValue,
  GenericContract,
  PointInfo (..),
  AuctionBid (..),
  AuctionState (..),
  AuctionOpenParams (..),
  AuctionBidParams (..),
  AuctionCloseParams (..),
  UniqueToken,
) where

import PlutusTx.Prelude
import Prelude qualified as Hask

import Plutus.Contract (Contract)

import Data.Monoid (Last (..))

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Ledger (
  Address,
  AssetClass,
  ChainIndexTxOut,
  CurrencySymbol,
  POSIXTime,
  PubKeyHash,
  TxOutRef,
 )

import Data.OpenApi.Schema qualified as OpenApi
import Ledger.Value (TokenName (..), unAssetClass)
import Plutus.ChainIndex (ChainIndexTx)
import PlutusTx qualified
import Schema (ToSchema)

--------------------------------------------------------------------------------
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
newtype NftId = NftId
  { -- | token name is identified by content of the NFT (it's hash of it).
    nftId'contentHash :: BuiltinByteString
  }
  deriving stock (Hask.Show, Generic, Hask.Eq, Hask.Ord)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

instance Eq NftId where
  {-# INLINEABLE (==) #-}
  (NftId token1) == (NftId token2) =
    token1 == token2
instance Ord NftId where
  {-# INLINEABLE (<=) #-}
  (NftId token1) <= (NftId token2) =
    token1 <= token2

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

-- | Minting Policy Redeemer
data MintAct
  = -- | Mint Action for the NftId. Creates a proof token that the NFTid is
    -- unique.
    Mint
      { -- | NftId
        mint'nftId :: !NftId
      }
  | -- | Create the Datum.
    Initialise
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- ENDPOINTS PARAMETERS --

-- | Parameters that need to be submitted when minting a new NFT.
data MintParams = MintParams
  { -- | Content to be minted.
    mp'content :: Content
  , -- | Title of content.
    mp'title :: Title
  , -- | Shares retained by author.
    mp'share :: Rational
  , -- | Listing price of the NFT, in Lovelace.
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
  { -- | NFTid of the token which price is set.
    sp'nftId :: NftId
  , -- | New price, in Lovelace.
    sp'price :: Maybe Integer -- todo maybe Natural? are they available here?
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
  , -- | price to buy, in Lovelace.
    ur'price :: Integer
  , -- | new price for NFT (Nothing locks NFT), in Lovelace.
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

data AuctionOpenParams = AuctionOpenParams
  { -- | nftId
    op'nftId :: NftId
  , -- | Auction deadline
    op'deadline :: POSIXTime
  , -- | Auction minimum bid in lovelace
    op'minBid :: Integer
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''AuctionOpenParams
PlutusTx.makeLift ''AuctionOpenParams

instance Eq AuctionOpenParams where
  {-# INLINEABLE (==) #-}
  (AuctionOpenParams nftId1 deadline1 minBid1) == (AuctionOpenParams nftId2 deadline2 minBid2) =
    nftId1 == nftId2 && deadline1 == deadline2 && minBid1 == minBid2

data AuctionBidParams = AuctionBidParams
  { -- | nftId
    bp'nftId :: NftId
  , -- | Bid amount in lovelace
    bp'bidAmount :: Integer
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''AuctionBidParams
PlutusTx.makeLift ''AuctionBidParams

instance Eq AuctionBidParams where
  {-# INLINEABLE (==) #-}
  (AuctionBidParams nftId1 bid1) == (AuctionBidParams nftId2 bid2) =
    nftId1 == nftId2 && bid1 == bid2

newtype AuctionCloseParams = AuctionCloseParams
  { -- | nftId
    cp'nftId :: NftId
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''AuctionCloseParams
PlutusTx.makeLift ''AuctionCloseParams

instance Eq AuctionCloseParams where
  {-# INLINEABLE (==) #-}
  (AuctionCloseParams nftId1) == (AuctionCloseParams nftId2) =
    nftId1 == nftId2

--------------------------------------------------------------------------------
-- Validation

data AuctionBid = AuctionBid
  { -- | Bid in Lovelace
    ab'bid :: Integer
  , -- | Bidder's wallet pubkey
    ab'bidder :: UserId
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''AuctionBid
PlutusTx.makeLift ''AuctionBid

instance Eq AuctionBid where
  {-# INLINEABLE (==) #-}
  (AuctionBid bid1 bidder1) == (AuctionBid bid2 bidder2) =
    bid1 == bid2 && bidder1 == bidder2

data AuctionState = AuctionState
  { -- | Highest bid
    as'highestBid :: Maybe AuctionBid
  , -- | Deadline
    as'deadline :: POSIXTime
  , -- | Minimum bid amount
    as'minBid :: Integer
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''AuctionState
PlutusTx.makeLift ''AuctionState

instance Eq AuctionState where
  {-# INLINEABLE (==) #-}
  (AuctionState bid1 deadline1 minBid1) == (AuctionState bid2 deadline2 minBid2) =
    bid1 == bid2 && deadline1 == deadline2 && minBid1 == minBid2

-- | NFT Information.
data InformationNft = InformationNft
  { -- | NFT ID. Represents the key of the Datum. ?could even be taken out of the information?
    info'id :: NftId
  , -- | Author's share of the NFT.
    info'share :: Rational
  , -- | Author's wallet pubKey.
    info'author :: UserId
  , -- | Owner's wallet pubkey.
    info'owner :: UserId
  , -- | Price in Lovelace. If Nothing, NFT not for sale.
    info'price :: Maybe Integer
  , -- | Auction state
    info'auctionState :: Maybe AuctionState
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''MintAct
PlutusTx.unstableMakeIsData ''NftId

PlutusTx.makeLift ''MintAct
PlutusTx.makeLift ''NftId

instance Ord InformationNft where
  x <= y = info'id x <= info'id y

PlutusTx.unstableMakeIsData ''InformationNft
PlutusTx.makeLift ''InformationNft
instance Eq InformationNft where
  {-# INLINEABLE (==) #-}
  (InformationNft a b c d e f) == (InformationNft a' b' c' d' e' f') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f'

-- | Unique Token is an AssetClass.
type UniqueToken = AssetClass

{- | App Instace is parametrised by the Unique Token located in the head of the
 list.
-}
data NftAppInstance = NftAppInstance
  { -- | Script Address where all the NFTs can be found
    appInstance'Address :: Address
  , -- | AssetClass with which all the NFTs are parametrised - guarantees the proof of uniqueness.
    appInstance'AppAssetClass :: UniqueToken
  , -- | Governance Address
    appInstance'Governance :: Address
  , -- | List of admins who can initiate the application
    appInstance'Admins :: [UserId]
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)

-- | Get `CurrencySumbol` of `NftAppInstance`
instanceCurrency :: NftAppInstance -> CurrencySymbol
instanceCurrency = fst . unAssetClass . appInstance'AppAssetClass

PlutusTx.unstableMakeIsData ''NftAppInstance
PlutusTx.makeLift ''NftAppInstance
instance Eq NftAppInstance where
  (NftAppInstance a b c d) == (NftAppInstance a' b' c' d') = a == a' && b == b' && c == c' && d == d'

newtype NftAppSymbol = NftAppSymbol {app'symbol :: CurrencySymbol}
  deriving stock (Hask.Show, Generic, Hask.Eq, Hask.Ord)
  deriving anyclass (ToJSON, FromJSON, OpenApi.ToSchema)

PlutusTx.unstableMakeIsData ''NftAppSymbol
PlutusTx.makeLift ''NftAppSymbol

instance Eq NftAppSymbol where
  (NftAppSymbol a) == (NftAppSymbol a') = a == a'

{- | The AssetClass is the pointer itself. Each NFT has the same CurrencySymbol,
 and their TokenName is the Hash of their Content.
-}
newtype Pointer = Pointer
  { pointer'assetClass :: AssetClass
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''Pointer
PlutusTx.makeLift ''Pointer

instance Eq Pointer where
  (Pointer a) == (Pointer a') = a == a'

instance Ord Pointer where
  (Pointer a) `compare` (Pointer a') = a `compare` a'

{- | The head datum is unique for each list. Its token is minted when the unique
 NFT is consumed.
-}
data NftListHead = NftListHead
  { -- | Pointer to the next node.
    head'next :: Maybe Pointer
  , -- | Node App Instance
    head'appInstance :: NftAppInstance
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''NftListHead
PlutusTx.makeLift ''NftListHead
instance Eq NftListHead where
  (NftListHead a b) == (NftListHead a' b') = a == a' && b == b'

-- | The nft list node is based on the above described properties.
data NftListNode = NftListNode
  { -- | The value held at the node
    node'information :: InformationNft
  , -- | The next Node.
    node'next :: Maybe Pointer
  , -- | Node App Instance
    node'appInstance :: NftAppInstance
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)

instance Ord NftListNode where
  x <= y = node'information x <= node'information y

PlutusTx.unstableMakeIsData ''NftListNode
PlutusTx.makeLift ''NftListNode
instance Eq NftListNode where
  (NftListNode a b c) == (NftListNode a' b' c') = a == a' && b == b' && c == c'

-- | The datum of an Nft is either head or node.
data DatumNft
  = -- | Head of a List
    HeadDatum NftListHead
  | -- | A node of the list.
    NodeDatum NftListNode
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)

instance Ord DatumNft where
  (HeadDatum _) <= _ = True
  (NodeDatum _) <= (HeadDatum _) = False
  (NodeDatum x) <= (NodeDatum y) = x <= y

PlutusTx.unstableMakeIsData ''DatumNft
PlutusTx.makeLift ''DatumNft

-- | Pointer to the next List Item.
getDatumPointer :: DatumNft -> Maybe Pointer
getDatumPointer = \case
  HeadDatum listHead -> head'next listHead
  NodeDatum listNode -> node'next listNode

getDatumValue :: DatumNft -> BuiltinByteString
getDatumValue = \case
  HeadDatum _ -> ""
  NodeDatum listNode -> nftId'contentHash . info'id . node'information $ listNode

instance Eq DatumNft where
  {-# INLINEABLE (==) #-}
  (HeadDatum x1) == (HeadDatum x2) = x1 == x2
  (NodeDatum x1) == (NodeDatum x2) = x1 == x2
  _ == _ = False

{- | Token Name is represented by the HASH of the artwork. The Head TokenName is
the empty ByteString, smaller than any other ByteString, and is minted at the
intialisation of the app.
-}
nftTokenName :: DatumNft -> TokenName
nftTokenName = \case
  HeadDatum _ -> TokenName PlutusTx.Prelude.emptyByteString
  NodeDatum n -> TokenName . nftId'contentHash . info'id . node'information $ n

getAppInstance :: DatumNft -> NftAppInstance
getAppInstance = \case
  HeadDatum (NftListHead _ inst) -> inst
  NodeDatum (NftListNode _ _ inst) -> inst

-- | NFT Redeemer
data UserAct
  = -- | Buy NFT and set new price
    BuyAct
      { -- | price to buy. In Lovelace.
        act'bid :: Integer
      , -- | new price for NFT. In Lovelace.
        act'newPrice :: Maybe Integer
      , -- | Nft symbol
        act'symbol :: NftAppSymbol
      }
  | -- | Set new price for NFT
    SetPriceAct
      { -- | new price for NFT. In Lovelace.
        act'newPrice :: Maybe Integer
      , -- | Nft symbol
        act'symbol :: NftAppSymbol
      }
  | -- | Mint a new Unique NFT.
    MintAct
      { -- | NFT.
        act'nftId :: NftId
      , -- | Nft symbol
        act'symbol :: NftAppSymbol
      }
  | -- | Start NFT auction
    OpenAuctionAct
      { -- | Nft symbol
        act'symbol :: NftAppSymbol
      }
  | -- | Make a bid in an auction
    BidAuctionAct
      { -- | Bid amount in lovelace
        act'bid :: Integer
      , -- | Nft symbol
        act'symbol :: NftAppSymbol
      }
  | CloseAuctionAct
      { -- | Nft symbol
        act'symbol :: NftAppSymbol
      }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''UserAct
PlutusTx.unstableMakeIsData ''UserAct

instance Eq UserAct where
  {-# INLINEABLE (==) #-}
  (BuyAct bid1 newPrice1 symbol1) == (BuyAct bid2 newPrice2 symbol2) =
    bid1 == bid2 && newPrice1 == newPrice2 && symbol1 == symbol2
  (SetPriceAct newPrice1 symbol1) == (SetPriceAct newPrice2 symbol2) =
    newPrice1 == newPrice2 && symbol1 == symbol2
  _ == _ = False

-- OffChain utility types.

-- | A datatype used by the QueryContract to return a response
data QueryResponse
  = QueryCurrentOwner (Maybe UserId)
  | QueryCurrentPrice (Maybe Integer)
  | QueryContent (Maybe InformationNft)
  | QueryListNfts [InformationNft]
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Easy type to find and use Nodes by.
data PointInfo = PointInfo
  { pi'datum :: DatumNft
  , pi'TOR :: TxOutRef
  , pi'CITxO :: ChainIndexTxOut
  , pi'CITx :: ChainIndexTx
  }
  deriving stock (Hask.Eq, Hask.Show)

instance Eq PointInfo where
  {-# INLINEABLE (==) #-}
  (PointInfo x y _ _) == (PointInfo a b _ _) =
    x == a && y == b -- && z == c && k == d

instance Ord PointInfo where
  x <= y = pi'datum x <= pi'datum y

instance Hask.Ord PointInfo where
  x <= y = pi'datum x <= pi'datum y

-- Contract types
type GenericContract a = forall w s. Contract w s Text a
type UserWriter = Last (Either NftId QueryResponse)
type UserContract s a = Contract UserWriter s Text a
type AdminContract s a = Contract (Last NftAppSymbol) s Text a
