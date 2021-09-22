{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Plutus.Types.Marketplace where
import qualified PlutusTx
import  Prelude 
import GHC.Generics  (Generic)
import qualified Data.Aeson                                       as J
import Plutus.Types.Percentage (Percentage)
import           Ledger

data Marketplace =
  Marketplace
    { marketplaceOperator :: PubKeyHash,
      marketplaceGasFee :: Percentage
    }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (J.ToJSON, J.FromJSON)

PlutusTx.makeLift ''Marketplace
PlutusTx.unstableMakeIsData ''Marketplace