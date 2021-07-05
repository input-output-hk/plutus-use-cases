{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Helper for testing logic of lending pool
module Mlabs.Emulator.Script(
    Script
  , runScript
  , getCurrentTime
  , putAct
) where

import Prelude (Semigroup(..), Monoid(..), Applicative(..))

import Control.Monad.State.Strict

import Data.Foldable
import Data.Sequence (Seq)
import Data.Monoid (Sum(..))
import PlutusTx.Prelude hiding (Monoid(..), Semigroup(..), Functor, Applicative, toList)

import qualified Data.Sequence as Seq

-- | Collects user actions and allocates timestamps
type Script act = ScriptM act ()

-- | Auto-allocation of timestamps, monadic interface for collection of actions
newtype ScriptM act a = Script (State (St act) a)
  deriving newtype (Functor, Applicative, Monad, MonadState (St act))

-- | Script accumulator state.
data St act = St
  { st'acts  :: Seq act      -- ^ acts so far
  , st'time  :: Sum Integer  -- ^ current timestamp
  }

instance Semigroup (St a) where
  St a1 t1 <> St a2 t2 = St (a1 <> a2) (t1 <> t2)

instance Monoid (St a) where
  mempty = St mempty mempty

-- | Extract list of acts from the script
runScript :: Script act-> [act]
runScript (Script actions) =
  toList $ st'acts $ execState actions (St Seq.empty 0)

getCurrentTime :: ScriptM act Integer
getCurrentTime = gets (getSum . st'time)

putAct :: act -> Script act
putAct act =
  modify' (<> St (Seq.singleton act) (Sum 1))

