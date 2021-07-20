-- | Common input check functions
module Mlabs.Control.Check(
    isNonNegative
  , isPositive
  , isPositiveRational
  , isUnitRange
  , isPositiveRay
  , isUnitRangeRay
) where

import PlutusTx.Prelude
import Control.Monad.Except (MonadError(..))
import qualified PlutusTx.Ratio as R

import Mlabs.Data.Ray (Ray)
import qualified Mlabs.Data.Ray as Ray
import           Prelude (String)

{-# INLINABLE isNonNegative #-}
isNonNegative :: (Applicative m, MonadError String m) => String -> Integer -> m ()
isNonNegative msg val
  | val >= 0  = pure ()
  | otherwise = throwError $ msg <> " should be non-negative"

{-# INLINABLE isPositive #-}
isPositive :: (Applicative m, MonadError String m) => String -> Integer -> m ()
isPositive msg val
  | val > 0   = pure ()
  | otherwise = throwError $ msg <> " should be positive"

{-# INLINABLE isPositiveRational #-}
isPositiveRational :: (Applicative m, MonadError String m) => String -> Rational -> m ()
isPositiveRational msg val
  | val > R.fromInteger 0 = pure ()
  | otherwise             = throwError $ msg <> " should be positive"

{-# INLINABLE isUnitRange #-}
isUnitRange :: (Applicative m, MonadError String m) => String -> Rational -> m ()
isUnitRange msg val
  | val >= R.fromInteger 0 && val <= R.fromInteger 1 = pure ()
  | otherwise                                        = throwError $ msg <> " should have unit range [0, 1]"

{-# INLINABLE isPositiveRay #-}
isPositiveRay :: (Applicative m, MonadError String m) => String -> Ray -> m ()
isPositiveRay msg val
  | val > Ray.fromInteger 0 = pure ()
  | otherwise               = throwError $ msg <> " should be positive"

{-# INLINABLE isUnitRangeRay #-}
isUnitRangeRay :: (Applicative m, MonadError String m) => String -> Ray -> m ()
isUnitRangeRay msg val
  | val >= Ray.fromInteger 0 && val <= Ray.fromInteger 1 = pure ()
  | otherwise                                            = throwError $ msg <> " should have unit range [0, 1]"

