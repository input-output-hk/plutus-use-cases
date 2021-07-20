{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fobject-code #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
-- | Ray math
--
-- We can represent fractional units with integers with 27 decimals precision
module Mlabs.Data.Ray(
    Ray(..)
  , fromInteger
  , (%)
  , fromRational
  , toRational
  , recip
  , round
  , properFraction
) where

import PlutusTx.Prelude hiding (fromInteger, fromRational, recip, (%), round, properFraction, toRational)

import Data.Aeson ( FromJSON, ToJSON )
import GHC.Generics ( Generic )
import Playground.Contract (ToSchema)
import PlutusCore.Default (DefaultUni)
import PlutusTx (IsData, Lift)
import PlutusTx.Ratio qualified as R
import Prelude qualified as Hask

{-# INLINABLE base #-}
-- | Base precision (27 precision digits are allowed)
base :: Integer
base = 1_000_000_000_000_000_000_000_000_000

{-# INLINABLE squareBase #-}
-- | base * base
squareBase :: Integer
squareBase = 1_000_000_000_000_000_000_000_000_000_000_000_000_000_000_000_000_000_000

-- | We represent fractionals with 27 precision
newtype Ray = Ray Integer
  deriving stock (Hask.Show, Generic)
  deriving newtype ( AdditiveSemigroup, AdditiveMonoid, AdditiveGroup
                   , Eq, Ord
                   , Hask.Eq, Hask.Ord
                   , FromJSON, ToJSON
                   , IsData
                   , Lift DefaultUni
                   , ToSchema)

instance MultiplicativeSemigroup Ray where
  {-# INLINABLE (*) #-}
  (*) (Ray a) (Ray b) = Ray $ (a * b * base) `divide` squareBase

instance MultiplicativeMonoid Ray where
  {-# INLINABLE one #-}
  one = Ray base

{-# INLINABLE (%) #-}
-- | Construct Ray as rationals
(%) :: Integer -> Integer -> Ray
(%) a b = fromRational (a R.% b)

{-# INLINABLE fromInteger #-}
-- | Convert from Integer.
fromInteger :: Integer -> Ray
fromInteger n = Ray (n * base)

{-# INLINABLE fromRational #-}
-- | Convert from Rational
fromRational :: Rational -> Ray
fromRational r = Ray $ (R.numerator r * base) `divide` R.denominator r

{-# INLINABLE toRational #-}
toRational :: Ray -> Rational
toRational (Ray a) = R.reduce a base

{-# INLINABLE recip #-}
-- | Reciprocal of ray.
--
-- equals to: base * base / ray
recip :: Ray -> Ray
recip (Ray a) = Ray (squareBase `divide` a)

{-# INLINABLE round #-}
-- | Round ray.
round :: Ray -> Integer
round (Ray a) = a `divide` base

{-# INLINABLE properFraction #-}
properFraction :: Ray -> (Integer, Ray)
properFraction (Ray a) = (d, Ray m)
  where
    (d, m) = Hask.divMod a base

