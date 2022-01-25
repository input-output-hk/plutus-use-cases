module Mlabs.EfficientNFT.Api (
  ApiUserContract,
  endpoints,
  NFTAppSchema,
) where

import Plutus.Contract (Contract, Endpoint, Promise, endpoint, type (.\/))

import Data.Monoid (Last (..))
import Data.Text (Text)
import Plutus.V1.Ledger.Value (AssetClass)

import Mlabs.EfficientNFT.Contract.ChangeOwner (changeOwner)
import Mlabs.EfficientNFT.Contract.MarketplaceBuy (marketplaceBuy)
import Mlabs.EfficientNFT.Contract.MarketplaceDeposit (marketplaceDeposit)
import Mlabs.EfficientNFT.Contract.MarketplaceRedeem (marketplaceRedeem)
import Mlabs.EfficientNFT.Contract.MarketplaceSetPrice (marketplaceSetPrice)
import Mlabs.EfficientNFT.Contract.Mint (mint, mintWithCollection)
import Mlabs.EfficientNFT.Contract.SetPrice (setPrice)
import Mlabs.EfficientNFT.Types
import Mlabs.Plutus.Contract (selectForever)

-- | A common App schema works for now.
type NFTAppSchema =
  -- Author Endpoints
  Endpoint "mint" MintParams
    .\/ Endpoint "mint-with-collection" (AssetClass, MintParams)
    -- User Action Endpoints
    .\/ Endpoint "change-owner" ChangeOwnerParams
    .\/ Endpoint "set-price" SetPriceParams
    .\/ Endpoint "marketplace-deposit" NftData
    .\/ Endpoint "marketplace-redeem" NftData
    .\/ Endpoint "marketplace-buy" NftData
    .\/ Endpoint "marketplace-set-price" SetPriceParams

-- ENDPOINTS --

type ApiUserContract a = Contract (Last NftData) NFTAppSchema Text a

-- | User Endpoints .
endpoints :: ApiUserContract ()
endpoints = selectForever tokenEndpointsList

-- | List of User Promises.
tokenEndpointsList :: [Promise (Last NftData) NFTAppSchema Text ()]
tokenEndpointsList =
  [ endpoint @"mint" mint
  , endpoint @"mint-with-collection" mintWithCollection
  , endpoint @"change-owner" changeOwner
  , endpoint @"set-price" setPrice
  , endpoint @"marketplace-deposit" marketplaceDeposit
  , endpoint @"marketplace-redeem" marketplaceRedeem
  , endpoint @"marketplace-buy" marketplaceBuy
  , endpoint @"marketplace-set-price" marketplaceSetPrice
  ]
