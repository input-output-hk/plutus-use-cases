{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Common plutus instances for StateT
module Mlabs.Control.Monad.State(
    PlutusState
  , MonadError(..)
  , MonadState(..)
  , runStateT
  , gets
  , guardError
) where

import PlutusTx.Prelude
import Prelude (String)

import Control.Monad.Except       hiding (Functor(..))
import Control.Monad.State.Strict hiding (Functor(..))

-- | State update of plutus contracts
type PlutusState st = StateT st (Either String)

instance Functor (PlutusState st) where
  {-# INLINABLE fmap #-}
  fmap f (StateT a) = StateT $ fmap (\(v, st) -> (f v, st)) . a

instance Applicative (PlutusState st) where
  {-# INLINABLE pure #-}
  pure a = StateT (\st -> Right (a, st))

  {-# INLINABLE (<*>) #-}
  (StateT f) <*> (StateT a) = StateT $ \st -> case f st of
    Left err -> Left err
    Right (f1, st1) -> fmap (\(a1, st2) -> (f1 a1, st2)) $ a st1

------------------------------------------------

{-# INLINABLE guardError #-}
-- | Execute further if condition is True or throw error with
-- given error message.
guardError :: (Applicative m, MonadError String m) => String -> Bool -> m ()
guardError msg isTrue
  | isTrue    = pure ()
  | otherwise = throwError msg
