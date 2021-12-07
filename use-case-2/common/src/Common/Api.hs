{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Api where

import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Aeson.GADT.TH
import Data.Constraint.Extras.TH
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.Int
import Data.Map (Map)
import Data.Semigroup (First(..))
import Data.Text (Text)
import Data.Vessel
import GHC.Generics

import Common.Plutus.Contracts.Uniswap.Types
import Common.Schema

-- | These is the "view" of all live queries, created using Vessel
-- (https://hackage.haskell.org/package/vessel). See
-- https://github.com/obsidiansystems/vessel/blob/develop/tutorial/Tutorial.md
-- for a nice step-by-step introduction.
type DexV = Vessel Q

-- | GADT plugged into the Vessel map type, see above.
data Q (v :: (* -> *) -> *) where
  Q_ContractList :: Q (IdentityV (Map Int32 (First (Maybe Text))))
  Q_PooledTokens :: Q (IdentityV (First (Maybe [PooledToken])))
  Q_Pools :: Q (IdentityV (Map Text (First (Maybe LPool))))

-- | In Obelisk apps, we follow CQRS such that only these should have side
-- effects, and only return ephemeral data like failure/sucess related to the
-- command. The (live) queries defined elsewhere should return actual persisted
-- application data.
data Api :: * -> * where
  Api_Swap
    :: ContractInstanceId Text
    -> Coin AssetClass
    -> Coin AssetClass
    -> Amount Integer
    -> Amount Integer
    -> Api (Either Aeson.Value Aeson.Value)

  Api_Stake
    :: ContractInstanceId Text
    -> Coin AssetClass
    -> Coin AssetClass
    -> Amount Integer
    -> Amount Integer
    -> Api (Either String Aeson.Value)

  Api_RedeemLiquidity
    :: ContractInstanceId Text
    -> Coin AssetClass
    -> Coin AssetClass
    -> Amount Integer
    -> Api (Either String Aeson.Value)

  Api_CallFunds
    :: ContractInstanceId Text
    -> Api ()

  Api_CallPools
    :: ContractInstanceId Text
    -> Api ()

  Api_EstimateTransactionFee
    :: SmartContractAction
    -> Api Integer

  Api_BuildStaticSwapTransaction
    :: Text -- Change Address (Wallet Address)
    -> Api (Either String Text) -- Either Error (CBOR encoded transaction)

data SmartContractAction = SmartContractAction_Swap
  deriving (Eq, Ord, Show, Generic)

instance ToJSON SmartContractAction
instance FromJSON SmartContractAction

deriveJSONGADT ''Api
deriveArgDict ''Api

deriveArgDict ''Q
deriveJSONGADT ''Q
deriveGEq ''Q
deriveGCompare ''Q
deriveGShow ''Q
