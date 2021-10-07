{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Plutus.Abstract.Percentage where
import qualified Data.Aeson   as J
import           GHC.Generics (Generic)
import qualified PlutusTx
import           Prelude      hiding (Fractional)

type Fractional = (Integer, Integer)

newtype Percentage =
      Percentage
      {getPercentage :: Fractional}
      deriving stock (Eq, Show, Generic)
      deriving anyclass (J.ToJSON, J.FromJSON)

PlutusTx.makeLift ''Percentage
PlutusTx.unstableMakeIsData ''Percentage

mkPercentage :: Fractional -> Maybe Percentage
mkPercentage percentage@(numerator, denominator) =
      let roundedPercentage = abs $ numerator `div` denominator
      in
      if denominator /= 0 && 0 <= roundedPercentage && roundedPercentage <= 100
            then pure $ Percentage percentage
            else Nothing
