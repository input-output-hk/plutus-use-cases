module Mlabs.NFT.Api (
  ApiUserContract,
  ApiAdminContract,
  NFTAppSchema,
  schemas,
  endpoints,
  queryEndpoints,
  adminEndpoints,
  nftMarketUserEndpoints,
) where

import Data.Monoid (Last (..))
import Data.Text (Text)

import Control.Monad (void)

import Playground.Contract (mkSchemaDefinitions)
import Plutus.Contract (Contract, Endpoint, Promise, endpoint, type (.\/))
import Prelude as Hask

import Mlabs.NFT.Contract.BidAuction (bidAuction)
import Mlabs.NFT.Contract.Buy (buy)
import Mlabs.NFT.Contract.CloseAuction (closeAuction)
import Mlabs.NFT.Contract.Init (initApp)
import Mlabs.NFT.Contract.Mint (mint)
import Mlabs.NFT.Contract.OpenAuction (openAuction)
import Mlabs.NFT.Contract.Query (queryContent, queryCurrentOwner, queryCurrentPrice, queryListNfts)
import Mlabs.NFT.Contract.SetPrice (setPrice)
import Mlabs.NFT.Types (
  AdminContract,
  AuctionBidParams (..),
  AuctionCloseParams (..),
  AuctionOpenParams (..),
  BuyRequestUser (..),
  Content,
  MintParams (..),
  NftAppSymbol (..),
  NftId (..),
  SetPriceParams (..),
  UserContract,
  UserWriter,
 )
import Mlabs.Plutus.Contract (selectForever)

-- | A common App schema works for now.
type NFTAppSchema =
  -- Author Endpoint
  Endpoint "mint" MintParams
    -- User Action Endpoints
    .\/ Endpoint "buy" BuyRequestUser
    .\/ Endpoint "set-price" SetPriceParams
    -- Query Endpoints
    .\/ Endpoint "query-current-owner" NftId
    .\/ Endpoint "query-current-price" NftId
    .\/ Endpoint "query-list-nfts" ()
    .\/ Endpoint "query-content" Content
    -- Auction endpoints
    .\/ Endpoint "auction-open" AuctionOpenParams
    .\/ Endpoint "auction-bid" AuctionBidParams
    .\/ Endpoint "auction-close" AuctionCloseParams
    -- Admin Endpoint
    .\/ Endpoint "app-init" ()

mkSchemaDefinitions ''NFTAppSchema

-- ENDPOINTS --

type ApiUserContract a = UserContract NFTAppSchema a

type ApiAdminContract a = AdminContract NFTAppSchema a

-- | Utility function to create endpoints from promises.
mkEndpoints :: forall w s e a b. (b -> [Promise w s e a]) -> b -> Contract w s e a
mkEndpoints listCont = selectForever . listCont

-- | User Endpoints .
endpoints :: NftAppSymbol -> ApiUserContract ()
endpoints = mkEndpoints userEndpointsList

-- | Query Endpoints are used for Querying, with no on-chain tx generation.
queryEndpoints :: NftAppSymbol -> ApiUserContract ()
queryEndpoints = mkEndpoints queryEndpointsList

-- | Admin Endpoints
adminEndpoints :: ApiAdminContract ()
adminEndpoints = mkEndpoints (const adminEndpointsList) ()

-- | Endpoints for NFT marketplace user - combination of user and query endpoints.
nftMarketUserEndpoints :: NftAppSymbol -> ApiUserContract ()
nftMarketUserEndpoints = mkEndpoints (userEndpointsList <> queryEndpointsList)

-- | List of User Promises.
userEndpointsList :: NftAppSymbol -> [Promise UserWriter NFTAppSchema Text ()]
userEndpointsList appSymbol =
  [ endpoint @"mint" (mint appSymbol)
  , endpoint @"buy" (buy appSymbol)
  , endpoint @"set-price" (setPrice appSymbol)
  , endpoint @"auction-open" (openAuction appSymbol)
  , endpoint @"auction-close" (closeAuction appSymbol)
  , endpoint @"auction-bid" (bidAuction appSymbol)
  ]

-- | List of Query endpoints.
queryEndpointsList :: NftAppSymbol -> [Promise UserWriter NFTAppSchema Text ()]
queryEndpointsList appSymbol =
  [ endpoint @"query-current-price" (void . queryCurrentPrice appSymbol)
  , endpoint @"query-current-owner" (void . queryCurrentOwner appSymbol)
  , endpoint @"query-list-nfts" (void . const (queryListNfts appSymbol))
  , endpoint @"query-content" (void . queryContent appSymbol)
  ]

-- | List of admin endpoints.
adminEndpointsList :: [Promise (Last NftAppSymbol) NFTAppSchema Text ()]
adminEndpointsList =
  [ endpoint @"app-init" $ Hask.const initApp
  ]
