module Mlabs.NFT.Api (
  ApiUserContract,
  ApiAdminContract,
  NFTAppSchema,
  schemas,
  endpoints,
  queryEndpoints,
  adminEndpoints,
) where

--import Data.Monoid (Last (..))
--import Data.Text (Text)

import Control.Monad (void)

import Playground.Contract (mkSchemaDefinitions)
import Plutus.Contract (Endpoint, endpoint, type (.\/))
import Prelude as Hask

import Mlabs.NFT.Contract.BidAuction (bidAuction)
import Mlabs.NFT.Contract.Buy (buy)
import Mlabs.NFT.Contract.CloseAuction (closeAuction)
import Mlabs.NFT.Contract.Init (initApp)
import Mlabs.NFT.Contract.Mint (mint)
import Mlabs.NFT.Contract.OpenAuction (openAuction)
import Mlabs.NFT.Contract.Query (queryCurrentOwner, queryCurrentPrice, queryListNfts)
import Mlabs.NFT.Contract.SetPrice (setPrice)
import Mlabs.NFT.Types (AdminContract, AuctionBidParams (..), AuctionCloseParams (..), AuctionOpenParams (..), BuyRequestUser (..), MintParams (..), NftAppSymbol (..), NftId (..), SetPriceParams (..), UserContract)
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

-- | User Endpoints .
endpoints :: NftAppSymbol -> ApiUserContract ()
endpoints appSymbol =
  selectForever
    [ endpoint @"mint" (mint appSymbol)
    , endpoint @"buy" (buy appSymbol)
    , endpoint @"set-price" (setPrice appSymbol)
    , --, endpoint @"query-authentic-nft" NFTContract.queryAuthenticNFT
      endpoint @"auction-open" (openAuction appSymbol)
    , endpoint @"auction-close" (closeAuction appSymbol)
    , endpoint @"auction-bid" (bidAuction appSymbol)
    ]

-- | Admin Endpoints
adminEndpoints :: ApiAdminContract ()
adminEndpoints =
  selectForever
    [ endpoint @"app-init" $ Hask.const initApp
    ]

-- Query Endpoints are used for Querying, with no on-chain tx generation.
queryEndpoints :: NftAppSymbol -> ApiUserContract ()
queryEndpoints appSymbol =
  selectForever
    [ endpoint @"query-current-price" (void . queryCurrentPrice appSymbol)
    , endpoint @"query-current-owner" (void . queryCurrentOwner appSymbol)
    , endpoint @"query-list-nfts" (void . const (queryListNfts appSymbol))
    ]
