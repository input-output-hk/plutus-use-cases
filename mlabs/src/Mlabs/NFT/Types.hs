{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFT.Types (
  AdminContract,
  AuctionBid (..),
  AuctionBidParams (..),
  AuctionCloseParams (..),
  AuctionOpenParams (..),
  AuctionState (..),
  as'highestBid,
  as'deadline,
  as'minBid,
  BuyRequestUser (..),
  Content (..),
  getContent,
  DatumNft (..),
  GenericContract,
  getAppInstance,
  getDatumPointer,
  getDatumValue,
  InformationNft (..),
  info'id,
  info'share,
  info'author,
  info'price,
  info'owner,
  info'auctionState,
  InsertPoint (..),
  instanceCurrency,
  MintAct (..),
  mint'nftId,
  MintParams (..),
  NftAppInstance (..),
  appInstance'Governance,
  appInstance'Address,
  appInstance'UniqueToken,
  appInstance'Admins,
  NftAppSymbol (..),
  NftId (..),
  nftId'contentHash,
  NftListHead (..),
  head'next,
  head'appInstance,
  NftListNode (..),
  node'information,
  node'next,
  node'appInstance,
  nftTokenName,
  Pointer (..),
  pointer'assetClass,
  PointInfo (..),
  QueryResponse (..),
  SetPriceParams (..),
  Title (..),
  getTitle,
  UniqueToken,
  UserAct (..),
  act'bid,
  act'newPrice,
  act'symbol,
  UserContract,
  UserId (..),
  getUserId,
  UserWriter,
  InitParams (..),
  app'symbol,
) where

import PlutusTx qualified
import PlutusTx.Prelude
import Prelude qualified as Hask

import Data.Aeson (FromJSON, ToJSON)
import Data.Monoid (Last (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Ledger (
  ChainIndexTxOut,
  POSIXTime,
  PubKeyHash,
  TxOutRef,
 )
import Plutus.ChainIndex (ChainIndexTx)
import Plutus.Contract (Contract)

import Mlabs.NFT.Spooky (Address, AssetClass (..), CurrencySymbol, Spooky, TokenName (..), toSpooky, unAssetClass, unSpooky)
import Schema (ToSchema)

--------------------------------------------------------------------------------
-- ON-CHAIN TYPES --

newtype Content = Content {getContent' :: Spooky BuiltinByteString}
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''Content
PlutusTx.makeLift ''Content

instance Eq Content where
  {-# INLINEABLE (==) #-}
  (Content c1) == (Content c2) = c1 == c2

{-# INLINEABLE getContent #-}
getContent :: Content -> BuiltinByteString
getContent = unSpooky . getContent'

newtype Title = Title {getTitle' :: Spooky BuiltinByteString}
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''Title
PlutusTx.makeLift ''Title

instance Eq Title where
  {-# INLINEABLE (==) #-}
  (Title t1) == (Title t2) = t1 == t2

{-# INLINEABLE getTitle #-}
getTitle :: Title -> BuiltinByteString
getTitle = unSpooky . getTitle'

newtype UserId = UserId {getUserId' :: Spooky PubKeyHash}
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''UserId
PlutusTx.makeLift ''UserId

instance Eq UserId where
  {-# INLINEABLE (==) #-}
  (UserId u1) == (UserId u2) = u1 == u2

instance Ord UserId where
  {-# INLINEABLE (<=) #-}
  (UserId u1) <= (UserId u2) = unSpooky @PubKeyHash u1 <= unSpooky u2

instance Hask.Ord UserId where
  (UserId u1) <= (UserId u2) = unSpooky @PubKeyHash u1 <= unSpooky u2

{-# INLINEABLE getUserId #-}
getUserId :: UserId -> PubKeyHash
getUserId = unSpooky . getUserId'

{- | Unique identifier of NFT.
 The NftId contains a human readable title, the hashed information of the
 content and the utxo ref included when minting the token.
-}
newtype NftId = NftId
  { -- | token name is identified by content of the NFT (it's hash of it).
    nftId'contentHash' :: Spooky BuiltinByteString
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

PlutusTx.unstableMakeIsData ''NftId
PlutusTx.makeLift ''NftId

instance Eq NftId where
  {-# INLINEABLE (==) #-}
  (NftId token1) == (NftId token2) =
    token1 == token2

instance Ord NftId where
  {-# INLINEABLE (<=) #-}
  (NftId token1) <= (NftId token2) =
    unSpooky @BuiltinByteString token1 <= unSpooky token2

instance Hask.Ord NftId where
  (NftId token1) <= (NftId token2) =
    unSpooky @BuiltinByteString token1 <= unSpooky token2

{-# INLINEABLE nftId'contentHash #-}
nftId'contentHash :: NftId -> BuiltinByteString
nftId'contentHash = unSpooky . nftId'contentHash'

-- | Minting Policy Redeemer
data MintAct
  = -- | Mint Action for the NftId. Creates a proof token that the NFTid is
    -- unique.
    Mint
      { -- | NftId
        mint'nftId' :: Spooky NftId
      }
  | -- | Create the Datum.
    Initialise
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''MintAct
PlutusTx.makeLift ''MintAct

{-# INLINEABLE mint'nftId #-}
mint'nftId :: MintAct -> NftId
mint'nftId Mint {..} = unSpooky mint'nftId'
mint'nftId _ = error ()

--------------------------------------------------------------------------------
-- ENDPOINTS PARAMETERS --

-- | Parameters for initialising NFT marketplace
data InitParams = InitParams
  { -- | List of app admins
    ip'admins :: [UserId]
  , -- | Fee rate of transaction
    ip'feeRate :: Rational
  , -- | PKH where fee is sent
    ip'feePkh :: PubKeyHash
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

PlutusTx.makeLift ''InitParams
PlutusTx.unstableMakeIsData ''InitParams

instance Eq InitParams where
  (InitParams admins1 feeRate1 feePkh1) == (InitParams admins2 feeRate2 feePkh2) =
    admins1 == admins2 && feeRate1 == feeRate2 && feePkh1 == feePkh2

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
    ab'bid' :: Spooky Integer
  , -- | Bidder's wallet pubkey
    ab'bidder' :: Spooky UserId
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)
PlutusTx.unstableMakeIsData ''AuctionBid
PlutusTx.makeLift ''AuctionBid

instance Eq AuctionBid where
  {-# INLINEABLE (==) #-}
  (AuctionBid bid1 bidder1) == (AuctionBid bid2 bidder2) =
    bid1 == bid2 && bidder1 == bidder2

data AuctionState = AuctionState
  { -- | Highest bid
    as'highestBid' :: Spooky (Maybe AuctionBid)
  , -- | Deadline
    as'deadline' :: Spooky POSIXTime
  , -- | Minimum bid amount
    as'minBid' :: Spooky Integer
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)
PlutusTx.unstableMakeIsData ''AuctionState
PlutusTx.makeLift ''AuctionState

instance Eq AuctionState where
  {-# INLINEABLE (==) #-}
  (AuctionState bid1 deadline1 minBid1) == (AuctionState bid2 deadline2 minBid2) =
    bid1 == bid2 && deadline1 == deadline2 && minBid1 == minBid2

{-# INLINEABLE as'highestBid #-}
as'highestBid :: AuctionState -> Maybe AuctionBid
as'highestBid = unSpooky . as'highestBid'

{-# INLINEABLE as'deadline #-}
as'deadline :: AuctionState -> POSIXTime
as'deadline = unSpooky . as'deadline'

{-# INLINEABLE as'minBid #-}
as'minBid :: AuctionState -> Integer
as'minBid = unSpooky . as'minBid'

-- | NFT Information.
data InformationNft = InformationNft
  { -- | NFT ID. Represents the key of the Datum. ?could even be taken out of the information?
    info'id' :: Spooky NftId
  , -- | Author's share of the NFT.
    info'share' :: Spooky Rational
  , -- | Author's wallet pubKey.
    info'author' :: Spooky UserId
  , -- | Owner's wallet pubkey.
    info'owner' :: Spooky UserId
  , -- | Price in Lovelace. If Nothing, NFT not for sale.
    info'price' :: Spooky (Maybe Integer)
  , -- | Auction state
    info'auctionState' :: Spooky (Maybe AuctionState)
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''InformationNft
PlutusTx.makeLift ''InformationNft

{-# INLINEABLE info'id #-}
info'id :: InformationNft -> NftId
info'id = unSpooky . info'id'

{-# INLINEABLE info'share #-}
info'share :: InformationNft -> Rational
info'share = unSpooky . info'share'

{-# INLINEABLE info'author #-}
info'author :: InformationNft -> UserId
info'author = unSpooky . info'author'

{-# INLINEABLE info'owner #-}
info'owner :: InformationNft -> UserId
info'owner = unSpooky . info'owner'

{-# INLINEABLE info'price #-}
info'price :: InformationNft -> Maybe Integer
info'price = unSpooky . info'price'

{-# INLINEABLE info'auctionState #-}
info'auctionState :: InformationNft -> Maybe AuctionState
info'auctionState = unSpooky . info'auctionState'

instance Ord InformationNft where
  {-# INLINEABLE (<=) #-}
  x <= y = info'id x <= info'id y

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
    appInstance'Address' :: Spooky Address
  , -- | AssetClass with which all the NFTs are parametrised - guarantees the proof of uniqueness.
    appInstance'UniqueToken' :: Spooky UniqueToken
  , -- | Governance Address
    appInstance'Governance' :: Spooky Address
  , -- | List of admins who can initiate the application
    appInstance'Admins' :: Spooky [UserId]
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''NftAppInstance
PlutusTx.makeLift ''NftAppInstance

instance Eq NftAppInstance where
  {-# INLINEABLE (==) #-}
  (NftAppInstance a b c d) == (NftAppInstance a' b' c' d') = a == a' && b == b' && c == c' && d == d'

{-# INLINEABLE appInstance'Address #-}
appInstance'Address :: NftAppInstance -> Address
appInstance'Address = unSpooky . appInstance'Address'

{-# INLINEABLE appInstance'UniqueToken #-}
appInstance'UniqueToken :: NftAppInstance -> UniqueToken
appInstance'UniqueToken = unSpooky . appInstance'UniqueToken'

{-# INLINEABLE appInstance'Governance #-}
appInstance'Governance :: NftAppInstance -> Address
appInstance'Governance = unSpooky . appInstance'Governance'

{-# INLINEABLE appInstance'Admins #-}
appInstance'Admins :: NftAppInstance -> [UserId]
appInstance'Admins = unSpooky . appInstance'Admins'

-- | Get `CurrencySumbol` of `NftAppInstance`
instanceCurrency :: NftAppInstance -> CurrencySymbol
instanceCurrency = fst . unAssetClass . appInstance'UniqueToken

newtype NftAppSymbol = NftAppSymbol {app'symbol' :: Spooky CurrencySymbol}
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)
PlutusTx.unstableMakeIsData ''NftAppSymbol
PlutusTx.makeLift ''NftAppSymbol

instance Eq NftAppSymbol where
  {-# INLINEABLE (==) #-}
  (NftAppSymbol a) == (NftAppSymbol a') = a == a'

{-# INLINEABLE app'symbol #-}
app'symbol :: NftAppSymbol -> CurrencySymbol
app'symbol = unSpooky . app'symbol'

{- | The AssetClass is the pointer itself. Each NFT has the same CurrencySymbol,
 and their TokenName is the Hash of their Content.
-}
newtype Pointer = Pointer
  { pointer'assetClass' :: Spooky AssetClass
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''Pointer
PlutusTx.makeLift ''Pointer

instance Eq Pointer where
  {-# INLINEABLE (==) #-}
  (Pointer a) == (Pointer a') = a == a'

instance Ord Pointer where
  {-# INLINEABLE compare #-}
  a `compare` a' = pointer'assetClass a `compare` pointer'assetClass a'

{-# INLINEABLE pointer'assetClass #-}
pointer'assetClass :: Pointer -> AssetClass
pointer'assetClass = unSpooky . pointer'assetClass'

{- | The head datum is unique for each list. Its token is minted when the unique
 NFT is consumed.
-}
data NftListHead = NftListHead
  { -- | Pointer to the next node.
    head'next' :: Spooky (Maybe Pointer)
  , -- | Node App Instance
    head'appInstance' :: Spooky NftAppInstance
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''NftListHead
PlutusTx.makeLift ''NftListHead

{-# INLINEABLE head'next #-}
head'next :: NftListHead -> Maybe Pointer
head'next = unSpooky . head'next'

{-# INLINEABLE head'appInstance #-}
head'appInstance :: NftListHead -> NftAppInstance
head'appInstance = unSpooky . head'appInstance'

instance Eq NftListHead where
  {-# INLINEABLE (==) #-}
  (NftListHead a b) == (NftListHead a' b') = a == a' && b == b'

-- | The nft list node is based on the above described properties.
data NftListNode = NftListNode
  { -- | The value held at the node
    node'information' :: Spooky InformationNft
  , -- | The next Node.
    node'next' :: Spooky (Maybe Pointer)
  , -- | Node App Instance
    node'appInstance' :: Spooky NftAppInstance
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)

instance Ord NftListNode where
  {-# INLINEABLE (<=) #-}
  x <= y = node'information x <= node'information y

{-# INLINEABLE node'information #-}
node'information :: NftListNode -> InformationNft
node'information = unSpooky . node'information'

{-# INLINEABLE node'next #-}
node'next :: NftListNode -> Maybe Pointer
node'next = unSpooky . node'next'

{-# INLINEABLE node'appInstance #-}
node'appInstance :: NftListNode -> NftAppInstance
node'appInstance = unSpooky . node'appInstance'

PlutusTx.unstableMakeIsData ''NftListNode
PlutusTx.makeLift ''NftListNode
instance Eq NftListNode where
  {-# INLINEABLE (==) #-}
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
  {-# INLINEABLE (<=) #-}
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
  HeadDatum _ -> TokenName . toSpooky $ PlutusTx.Prelude.emptyByteString
  NodeDatum n -> TokenName . toSpooky . nftId'contentHash . info'id . node'information $ n

getAppInstance :: DatumNft -> NftAppInstance
getAppInstance = \case
  HeadDatum h -> head'appInstance h
  NodeDatum n -> node'appInstance n

-- | NFT Redeemer
data UserAct
  = -- | Buy NFT and set new price
    BuyAct
      { -- | price to buy. In Lovelace.
        act'bid' :: Spooky Integer
      , -- | new price for NFT. In Lovelace.
        act'newPrice' :: Spooky (Maybe Integer)
      , -- | Nft symbol
        act'symbol' :: Spooky NftAppSymbol
      }
  | -- | Set new price for NFT
    SetPriceAct
      { -- | new price for NFT. In Lovelace.
        act'newPrice' :: Spooky (Maybe Integer)
      , -- | Nft symbol
        act'symbol' :: Spooky NftAppSymbol
      }
  | -- | Mint a new Unique NFT.
    MintAct
      { -- | NFT.
        act'nftId' :: Spooky NftId
      , -- | Nft symbol
        act'symbol' :: Spooky NftAppSymbol
      }
  | -- | Start NFT auction
    OpenAuctionAct
      { -- | Nft symbol
        act'symbol' :: Spooky NftAppSymbol
      }
  | -- | Make a bid in an auction
    BidAuctionAct
      { -- | Bid amount in lovelace
        act'bid' :: Spooky Integer
      , -- | Nft symbol
        act'symbol' :: Spooky NftAppSymbol
      }
  | CloseAuctionAct
      { -- | Nft symbol
        act'symbol' :: Spooky NftAppSymbol
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

{-# INLINEABLE act'bid #-}
act'bid :: UserAct -> Integer
act'bid (BuyAct bid _ _) = unSpooky bid
act'bid (BidAuctionAct bid _) = unSpooky bid
act'bid _ = error ()

{-# INLINEABLE act'newPrice #-}
act'newPrice :: UserAct -> Maybe Integer
act'newPrice (BuyAct _ newPrice _) = unSpooky newPrice
act'newPrice (SetPriceAct newPrice _) = unSpooky newPrice
act'newPrice _ = error ()

{-# INLINEABLE act'symbol #-}
act'symbol :: UserAct -> NftAppSymbol
act'symbol (BuyAct _ _ symbol) = unSpooky symbol
act'symbol (SetPriceAct _ symbol) = unSpooky symbol
act'symbol (MintAct _ symbol) = unSpooky symbol
act'symbol (OpenAuctionAct symbol) = unSpooky symbol
act'symbol (BidAuctionAct _ symbol) = unSpooky symbol
act'symbol (CloseAuctionAct symbol) = unSpooky symbol

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
data PointInfo a = PointInfo
  { pi'data :: a
  , pi'TOR :: Ledger.TxOutRef
  , pi'CITxO :: ChainIndexTxOut
  , pi'CITx :: ChainIndexTx
  }
  deriving stock (Hask.Eq, Hask.Show)

instance Eq a => Eq (PointInfo a) where
  {-# INLINEABLE (==) #-}
  (PointInfo x y _ _) == (PointInfo a b _ _) =
    x == a && y == b -- && z == c && k == d

instance Ord a => Ord (PointInfo a) where
  {-# INLINEABLE (<=) #-}
  x <= y = pi'data x <= pi'data y

instance (Ord a, Hask.Eq a) => Hask.Ord (PointInfo a) where
  x <= y = pi'data x <= pi'data y

-- | Two positions in on-chain list between which new NFT will be "inserted"
data InsertPoint a = InsertPoint
  { prev :: PointInfo a
  , next :: Maybe (PointInfo a)
  }
  deriving stock (Hask.Show)

-- Contract types
type GenericContract a = forall w s. Contract w s Text a
type UserWriter = Last (Either NftId QueryResponse)
type UserContract s a = Contract UserWriter s Text a
type AdminContract s a = Contract (Last NftAppInstance) s Text a
