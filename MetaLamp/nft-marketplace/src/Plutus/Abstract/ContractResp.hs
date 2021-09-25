{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}

module Plutus.Abstract.ContractResp where

import qualified Data.Aeson                 as J
import qualified Data.Map.Strict            as Map
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Plutus.Abstract.RemoteData (RemoteData)
import qualified Test.QuickCheck           as Q
import qualified Test.QuickCheck.Instances           as Q

newtype ContractResp a =
  ContractResp
    { getContractResponses :: Map.Map String (RemoteData Text a)
    }
  deriving  (Eq, Show, Generic)
  deriving newtype (J.ToJSON, J.FromJSON, Q.Arbitrary)

instance Semigroup (ContractResp a) where
  (ContractResp x) <> (ContractResp y) = ContractResp $ Map.unionWith (<>) x y

instance Monoid (ContractResp a) where
  mempty = ContractResp mempty
