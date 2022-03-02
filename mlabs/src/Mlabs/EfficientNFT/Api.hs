module Mlabs.EfficientNFT.Api (
  ApiUserContract,
  endpoints,
  NFTAppSchema,
) where

import Plutus.Contract (Contract, Endpoint, Promise, endpoint, type (.\/))

import Data.Monoid (Last (..))
import Data.Text (Text)
import Plutus.V1.Ledger.Value (AssetClass)

import Control.Monad (void)
import Mlabs.EfficientNFT.Contract.Burn (burn)
import Mlabs.EfficientNFT.Contract.ChangeOwner (changeOwner)
import Mlabs.EfficientNFT.Contract.MarketplaceBuy (marketplaceBuy)
import Mlabs.EfficientNFT.Contract.MarketplaceDeposit (marketplaceDeposit)
import Mlabs.EfficientNFT.Contract.MarketplaceRedeem (marketplaceRedeem)
import Mlabs.EfficientNFT.Contract.MarketplaceSetPrice (marketplaceSetPrice)
import Mlabs.EfficientNFT.Contract.Mint (mint, mintWithCollection)
import Mlabs.EfficientNFT.Contract.SetPrice (setPrice)
import Mlabs.EfficientNFT.Types
import Mlabs.Plutus.Contract (selectForever)
import PlutusTx.Prelude

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
    .\/ Endpoint "burn" NftData

-- ENDPOINTS --

type ApiUserContract a = Contract (Last NftData) NFTAppSchema Text a

-- | User Endpoints .
endpoints :: ApiUserContract ()
endpoints = selectForever tokenEndpointsList

-- | List of User Promises.
tokenEndpointsList :: [Promise (Last NftData) NFTAppSchema Text ()]
tokenEndpointsList =
  [ void $ endpoint @"mint" mint
  , void $ endpoint @"mint-with-collection" mintWithCollection
  , endpoint @"change-owner" changeOwner
  , void $ endpoint @"set-price" setPrice
  , void $ endpoint @"marketplace-deposit" marketplaceDeposit
  , void $ endpoint @"marketplace-redeem" marketplaceRedeem
  , void $ endpoint @"marketplace-buy" marketplaceBuy
  , void $ endpoint @"marketplace-set-price" marketplaceSetPrice
  , endpoint @"burn" burn
  ]
