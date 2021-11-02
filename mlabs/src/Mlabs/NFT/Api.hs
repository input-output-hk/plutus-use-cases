module Mlabs.NFT.Api (
  NFTAppSchema,
  schemas,
  endpoints,
  queryEndpoints,
  adminEndpoints,
) where

import Data.Monoid (Last (..))
import Data.Text (Text)

import Playground.Contract (mkSchemaDefinitions)
import Plutus.Contract (Contract, Endpoint, endpoint, throwError, type (.\/))
import Prelude as Hask

import Mlabs.NFT.Contract.Buy (buy)
import Mlabs.NFT.Contract.Init (initApp)
import Mlabs.NFT.Contract.Mint (mint)
import Mlabs.NFT.Contract.SetPrice (setPrice)
import Mlabs.NFT.Types (BuyRequestUser (..), MintParams (..), NftAppSymbol (..), NftId (..), QueryResponse (..), SetPriceParams (..))
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
    .\/ Endpoint "query-authentic-nft" NftId
    -- Admin Endpoint
    .\/ Endpoint "app-init" ()

mkSchemaDefinitions ''NFTAppSchema

-- ENDPOINTS --

type ApiUserContract a = Contract (Last NftId) NFTAppSchema Text a
type ApiAdminContract a = Contract (Last NftAppSymbol) NFTAppSchema Text a
type ApiQueryContract a = Contract QueryResponse NFTAppSchema Text a

-- | User Endpoints .
endpoints :: NftAppSymbol -> ApiUserContract ()
endpoints appSymbol =
  selectForever
    [ endpoint @"mint" (mint appSymbol)
    , endpoint @"buy" (buy appSymbol)
    , endpoint @"set-price" (setPrice appSymbol)
    --, endpoint @"query-authentic-nft" NFTContract.queryAuthenticNFT
    ]

-- | Admin Endpoints
adminEndpoints :: ApiAdminContract ()
adminEndpoints =
  selectForever
    [ endpoint @"app-init" $ Hask.const initApp
    ]

-- Query Endpoints are used for Querying, with no on-chain tx generation.
queryEndpoints :: ApiQueryContract ()
queryEndpoints = throwError "FIXME"

-- selectForever
--   [ endpoint @"query-current-price" NFTContract.queryCurrentPrice
--   , endpoint @"query-current-owner" NFTContract.queryCurrentOwner
--   ]
