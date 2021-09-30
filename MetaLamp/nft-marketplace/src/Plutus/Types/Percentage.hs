{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Plutus.Types.Percentage where
import qualified Data.Aeson     as J
import           GHC.Generics   (Generic)
import qualified PlutusTx
import           PlutusTx.Ratio  -- Ratio is from PlutusTx
import           Prelude

newtype Percentage =
      Percentage
      {getPercentage :: Ratio Integer}   -- Ratio is inside Percentage
      deriving stock (Eq, Show, Generic)
      deriving anyclass (J.ToJSON, J.FromJSON)

mkPercentage :: Ratio Integer -> Maybe Percentage
mkPercentage percentage =
      let decimal :: Double =
            (fromIntegral $ numerator percentage) /
            (fromIntegral $ denominator percentage)
      in
      if 0 <= decimal && decimal <= 100
            then pure $ Percentage percentage
            else Nothing

PlutusTx.makeLift ''Percentage
PlutusTx.unstableMakeIsData ''Percentage
