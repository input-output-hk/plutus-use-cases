{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Contract API for Lendex application
module Mlabs.NftStateMachine.Contract.Api (
  Buy (..),
  SetPrice (..),
  StartParams (..),
  UserSchema,
  AuthorSchema,
  IsUserAct (..),
) where

import GHC.Generics (Generic)
import Playground.Contract (FromJSON, ToJSON, ToSchema)
import Plutus.Contract (type (.\/))
import PlutusTx.Prelude (BuiltinByteString, Integer, Maybe, Rational)
import Prelude qualified as Hask (Eq, Show)

import Mlabs.NftStateMachine.Logic.Types (UserAct (BuyAct, SetPriceAct))
import Mlabs.Plutus.Contract (Call, IsEndpoint (..))

----------------------------------------------------------------------
-- NFT endpoints

-- user endpoints

-- | User buys NFT
data Buy = Buy
  { buy'price :: Integer
  , buy'newPrice :: Maybe Integer
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | User sets new price for NFT
newtype SetPrice = SetPrice
  { setPrice'newPrice :: Maybe Integer
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- author endpoints

-- | Parameters to init NFT
data StartParams = StartParams
  { -- | NFT content
    sp'content :: BuiltinByteString
  , -- | author share [0, 1] on reselling of the NFT
    sp'share :: Rational
  , -- | current price of NFT, if it's nothing then nobody can buy it.
    sp'price :: Maybe Integer
  }
  deriving stock (Hask.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

----------------------------------------------------------------------
-- schemas

-- | User schema. Owner can set the price and the buyer can try to buy.
type UserSchema =
  Call Buy
    .\/ Call SetPrice

-- | Schema for the author of NFT
type AuthorSchema =
  Call StartParams

----------------------------------------------------------------------
-- classes

class IsUserAct a where
  toUserAct :: a -> UserAct

instance IsUserAct Buy where toUserAct Buy {..} = BuyAct buy'price buy'newPrice
instance IsUserAct SetPrice where toUserAct SetPrice {..} = SetPriceAct setPrice'newPrice

instance IsEndpoint Buy where
  type EndpointSymbol Buy = "buy-nft"

instance IsEndpoint SetPrice where
  type EndpointSymbol SetPrice = "set-price-for-nft"

instance IsEndpoint StartParams where
  type EndpointSymbol StartParams = "start-nft"
