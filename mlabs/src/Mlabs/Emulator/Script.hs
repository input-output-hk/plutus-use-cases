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

import PlutusTx.Prelude

import Control.Monad.State.Strict qualified as Strict
--import Data.Foldable ( Foldable(toList) )
import Data.Monoid (Sum(..))
import Data.Sequence as Seq ( Seq, empty, singleton )
import PlutusTx.Prelude ( Integer, (.), ($) )

-- | Collects user actions and allocates timestamps
type Script act = ScriptM act ()

-- | Auto-allocation of timestamps, monadic interface for collection of actions
newtype ScriptM act a = Script (Strict.State (St act) a)
  deriving newtype (Strict.Functor, Applicative, Strict.Monad, Strict.MonadState (St act))

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
  toList $ st'acts $ Strict.execState actions (St empty 0)

getCurrentTime :: ScriptM act Integer
getCurrentTime = Strict.gets (getSum . st'time)

putAct :: act -> Script act
putAct act =
  Strict.modify' (<> St (singleton act) (Sum 1))

