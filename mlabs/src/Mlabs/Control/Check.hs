-- | Common input check functions
module Mlabs.Control.Check (
  isNonNegative,
  isPositive,
  isPositiveRational,
  isUnitRange,
) where

import Control.Monad.Except (MonadError (..))
import PlutusTx.Prelude
import PlutusTx.Ratio qualified as R

import Prelude (String)

{-# INLINEABLE isNonNegative #-}
isNonNegative :: (Applicative m, MonadError String m) => String -> Integer -> m ()
isNonNegative msg val
  | val >= 0 = pure ()
  | otherwise = throwError $ msg <> " should be non-negative"

{-# INLINEABLE isPositive #-}
isPositive :: (Applicative m, MonadError BuiltinByteString m) => BuiltinByteString -> Integer -> m ()
isPositive msg val
  | val > 0 = pure ()
  | otherwise = throwError $ msg <> " should be positive"

{-# INLINEABLE isPositiveRational #-}
isPositiveRational :: (Applicative m, MonadError String m) => String -> Rational -> m ()
isPositiveRational msg val
  | val > R.fromInteger 0 = pure ()
  | otherwise = throwError $ msg <> " should be positive"

{-# INLINEABLE isUnitRange #-}
isUnitRange :: (Applicative m, MonadError String m) => String -> Rational -> m ()
isUnitRange msg val
  | val >= R.fromInteger 0 && val <= R.fromInteger 1 = pure ()
  | otherwise = throwError $ msg <> " should have unit range [0, 1]"
