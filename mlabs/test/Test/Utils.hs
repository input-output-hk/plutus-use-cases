module Test.Utils(
    throwError
  , next
  , wait
  , concatPredicates
) where


import Data.Functor (void)
import Plutus.Contract.Test

import qualified Plutus.Trace.Emulator as Trace
import qualified Data.List as L

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
concatPredicates = L.foldl1' (.&&.)


