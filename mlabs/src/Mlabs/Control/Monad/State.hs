{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Common plutus instances for StateT
module Mlabs.Control.Monad.State (
  PlutusState,
  MonadError (..),
  MonadState (..),
  runStateT,
  gets,
  guardError,
) where

import PlutusTx.Prelude
import Prelude (String)

import Control.Monad.Except (MonadError (..))
import Control.Monad.State.Strict (MonadState (..), StateT (..), gets)

-- | State update of plutus contracts
type PlutusState st = StateT st (Either String)

instance Functor (PlutusState st) where
  {-# INLINEABLE fmap #-}
  fmap f (StateT a) = StateT $ fmap g . a
    where
      g (v, st) = (f v, st)

instance Applicative (PlutusState st) where
  {-# INLINEABLE pure #-}
  pure a = StateT (\st -> Right (a, st))

  {-# INLINEABLE (<*>) #-}
  (StateT f) <*> (StateT a) = StateT $ \st -> case f st of
    Left err -> Left err
    Right (f1, st1) -> do
      (a1, st2) <- a st1
      return (f1 a1, st2)

------------------------------------------------

{-# INLINEABLE guardError #-}

{- | Execute further if condition is True or throw error with
 given error message.
-}
guardError :: (Applicative m, MonadError String m) => String -> Bool -> m ()
guardError msg isTrue
  | isTrue = pure ()
  | otherwise = throwError msg
