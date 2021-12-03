module Mlabs.NFT.Api (
  adminEndpoints,
  ApiAdminContract,
  ApiUserContract,
  endpoints,
  NFTAppSchema,
  nftMarketUserEndpoints,
  queryEndpoints,
  schemas,
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
  InitParams (..),
  MintParams (..),
  NftAppInstance (..),
  NftId (..),
  SetPriceParams (..),
  UniqueToken,
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
    .\/ Endpoint "app-init" InitParams

mkSchemaDefinitions ''NFTAppSchema

-- ENDPOINTS --

type ApiUserContract a = UserContract NFTAppSchema a

type ApiAdminContract a = AdminContract NFTAppSchema a

-- | Utility function to create endpoints from promises.
mkEndpoints :: forall w s e a b. (b -> [Promise w s e a]) -> b -> Contract w s e a
mkEndpoints listCont = selectForever . listCont

-- | User Endpoints .
endpoints :: UniqueToken -> ApiUserContract ()
endpoints = mkEndpoints userEndpointsList

-- | Query Endpoints are used for Querying, with no on-chain tx generation.
queryEndpoints :: UniqueToken -> ApiUserContract ()
queryEndpoints = mkEndpoints queryEndpointsList

-- | Admin Endpoints
adminEndpoints :: ApiAdminContract ()
adminEndpoints = mkEndpoints (const adminEndpointsList) ()

-- | Endpoints for NFT marketplace user - combination of user and query endpoints.
nftMarketUserEndpoints :: UniqueToken -> ApiUserContract ()
nftMarketUserEndpoints = mkEndpoints (userEndpointsList <> queryEndpointsList)

-- | List of User Promises.
userEndpointsList :: UniqueToken -> [Promise UserWriter NFTAppSchema Text ()]
userEndpointsList uT =
  [ endpoint @"mint" (mint uT)
  , endpoint @"buy" (buy uT)
  , endpoint @"set-price" (setPrice uT)
  , endpoint @"auction-open" (openAuction uT)
  , endpoint @"auction-close" (closeAuction uT)
  , endpoint @"auction-bid" (bidAuction uT)
  ]

-- | List of Query endpoints.
queryEndpointsList :: UniqueToken -> [Promise UserWriter NFTAppSchema Text ()]
queryEndpointsList uT =
  [ endpoint @"query-current-price" (void . queryCurrentPrice uT)
  , endpoint @"query-current-owner" (void . queryCurrentOwner uT)
  , endpoint @"query-list-nfts" (void . const (queryListNfts uT))
  , endpoint @"query-content" (void . queryContent uT)
  ]

-- | List of admin endpoints.
adminEndpointsList :: [Promise (Last NftAppInstance) NFTAppSchema Text ()]
adminEndpointsList =
  [ endpoint @"app-init" initApp
  ]
