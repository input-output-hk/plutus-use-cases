{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Plutus.Types.Percentage where
import qualified PlutusTx
import  Prelude 
import GHC.Generics  (Generic)
import qualified Data.Aeson                                       as J
import PlutusTx.Ratio

newtype Percentage = 
      Percentage 
      {getPercentage :: Ratio Integer} 
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
