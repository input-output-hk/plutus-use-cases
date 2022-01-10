module Mlabs.EfficientNFT.Api (
  ApiUserContract,
  endpoints,
  NFTAppSchema,
) where

import Plutus.Contract (Contract, Endpoint, Promise, endpoint, type (.\/))
import Prelude as Hask

import Data.Monoid (Last (..))
import Data.Text (Text)

import Mlabs.EfficientNFT.Contract.ChangeOwner (changeOwner)
import Mlabs.EfficientNFT.Contract.Mint (mint)
import Mlabs.EfficientNFT.Contract.SetPrice (setPrice)
import Mlabs.EfficientNFT.Token
import Mlabs.EfficientNFT.Types
import Mlabs.Plutus.Contract (selectForever)

-- | A common App schema works for now.
type NFTAppSchema =
  -- Author Endpoint
  Endpoint "mint" MintParams
    -- User Action Endpoints
    .\/ Endpoint "change-owner" ChangeOwnerParams
    .\/ Endpoint "set-price" SetPriceParams

-- ENDPOINTS --

type ApiUserContract a = Contract (Last NftId) NFTAppSchema Text a

-- | Utility function to create endpoints from promises.
mkEndpoints :: forall w s e a b. (b -> [Promise w s e a]) -> b -> Contract w s e a
mkEndpoints listCont = selectForever . listCont

-- | User Endpoints .
endpoints :: PlatformConfig -> ApiUserContract ()
endpoints = mkEndpoints tokenEndpointsList

-- | List of User Promises.
tokenEndpointsList :: PlatformConfig -> [Promise (Last NftId) NFTAppSchema Text ()]
tokenEndpointsList pc =
  [ endpoint @"mint" (mint pc)
  , endpoint @"change-owner" (changeOwner pc)
  , endpoint @"set-price" (setPrice pc)
  ]
