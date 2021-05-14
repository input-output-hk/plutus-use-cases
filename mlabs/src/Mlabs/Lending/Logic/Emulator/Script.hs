-- | Helper for testing logic of lending pool
module Mlabs.Lending.Logic.Emulator.Script(
    Script
  , runScript
  , userAct
  , priceAct
  , governAct
) where

import Prelude (Semigroup(..), Monoid(..), Applicative(..))

import Control.Monad.State.Strict

import Data.Foldable
import Data.Sequence (Seq)
import Data.Monoid (Sum(..))
import PlutusTx.Prelude hiding (Monoid(..), Semigroup(..), Functor, Applicative, toList)

import Mlabs.Lending.Logic.Types
import qualified Data.Sequence as Seq

-- | Collects user actions and allocates timestamps
type Script = ScriptM ()

-- | Auto-allocation of timestamps, monadic interface for collection of actions
newtype ScriptM a = Script (State St a)
  deriving newtype (Functor, Applicative, Monad, MonadState St)

-- | Script accumulator state.
data St = St
  { st'acts  :: Seq Act      -- ^ acts so far
  , st'time  :: Sum Integer  -- ^ current timestamp
  }

instance Semigroup St where
  St a1 t1 <> St a2 t2 = St (a1 <> a2) (t1 <> t2)

instance Monoid St where
  mempty = St mempty mempty

-- | Extract list of acts from the script
runScript :: Script -> [Act]
runScript (Script actions) =
  toList $ st'acts $ execState actions (St Seq.empty 0)

-- | Make user act
userAct :: UserId -> UserAct -> Script
userAct uid act = do
  time <- gets (getSum . st'time)
  putAct $ UserAct time uid act

-- | Make price act
priceAct :: PriceAct -> Script
priceAct arg = putAct $ PriceAct arg

-- | Make govern act
governAct :: GovernAct -> Script
governAct arg = putAct $ GovernAct arg

putAct :: Act -> Script
putAct act =
  modify' (<> St (Seq.singleton act) (Sum 1))

