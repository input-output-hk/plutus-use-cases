{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Plutus.Abstract.Percentage where
import qualified Data.Aeson          as J
import qualified Data.OpenApi.Schema as OpenApi
import           GHC.Generics        (Generic)
import qualified PlutusTx
import           Prelude             hiding (Fractional)
import qualified Schema

type Fractional = (Integer, Integer)

newtype Percentage =
      Percentage
      {getPercentage :: Fractional}
      deriving stock (Eq, Show, Ord, Generic)
      deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema, OpenApi.ToSchema)

PlutusTx.makeLift ''Percentage
PlutusTx.unstableMakeIsData ''Percentage

mkPercentage :: Fractional -> Maybe Percentage
mkPercentage percentage@(numerator, denominator) =
      let roundedPercentage = abs $ numerator `div` denominator
      in
      if denominator /= 0 && 0 <= roundedPercentage && roundedPercentage <= 100
            then pure $ Percentage percentage
            else Nothing

