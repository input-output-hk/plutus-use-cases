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
import Data.Semigroup (First(..))
import Data.Text (Text)
import Data.Vessel
import GHC.Generics

import Common.Plutus.Contracts.Uniswap.Types
import Common.Schema

commonStuff :: String
commonStuff = "Here is a string defined in Common.Api"

data SmartContractAction = SmartContractAction_Swap
  deriving (Eq, Ord, Show, Generic)

instance ToJSON SmartContractAction
instance FromJSON SmartContractAction

type DexV = Vessel Q

-- Note: This is view
data Q (v :: (* -> *) -> *) where
  Q_ContractList :: Q (IdentityV (First (Maybe [Text])))
  Q_PooledTokens :: Q (IdentityV (First (Maybe [PooledToken])))

data Api :: * -> * where
  Api_Swap :: ContractInstanceId Text -> Coin AssetClass -> Coin AssetClass -> Amount Integer -> Amount Integer -> Api (Either String Aeson.Value)
  Api_Stake :: ContractInstanceId Text -> Coin AssetClass -> Coin AssetClass -> Amount Integer -> Amount Integer -> Api (Either String Aeson.Value)
  Api_RedeemLiquidity :: ContractInstanceId Text -> Coin AssetClass -> Coin AssetClass -> Amount Integer -> Api (Either String Aeson.Value)
  Api_CallFunds :: ContractInstanceId Text -> Api ()
  Api_CallPools :: ContractInstanceId Text -> Api ()
  Api_EstimateTransactionFee :: SmartContractAction -> Api Integer

deriveJSONGADT ''Api
deriveArgDict ''Api

deriveArgDict ''Q
deriveJSONGADT ''Q
deriveGEq ''Q
deriveGCompare ''Q
deriveGShow ''Q
