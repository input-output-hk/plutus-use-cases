module Test.Utils(
    throwError
  , next
  , wait
  , concatPredicates
) where

import PlutusTx.Prelude hiding (fromInteger)
import Prelude (String, fromInteger)

import Data.Functor (void)
import Data.List (foldl1')
import Plutus.Contract.Test ( TracePredicate, (.&&.) )
import qualified Plutus.Trace.Emulator as Trace

-- | Throws error to emulator trace.
throwError :: String -> Trace.EmulatorTrace a
throwError msg = Trace.throwError (Trace.GenericError msg)

-- | Wait for one slot.
next :: Trace.EmulatorTrace ()
next = void Trace.nextSlot

-- | Wait given amount of slots.
wait :: Integer -> Trace.EmulatorTrace ()
wait = void . Trace.waitNSlots . fromInteger

concatPredicates :: [TracePredicate] -> TracePredicate
concatPredicates = foldl1' (.&&.)

