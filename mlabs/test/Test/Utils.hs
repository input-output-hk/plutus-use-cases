module Test.Utils(
    throwError
  , next
  , wait
  , testNoErrors
) where


import Data.Functor (void)
import Test.Tasty.HUnit (assertFailure)

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

-- | Check that there are no errors during execution of the script.
testNoErrors :: Trace.EmulatorConfig -> Trace.EmulatorTrace () -> IO ()
testNoErrors cfg trace = case err of
  Just e  -> assertFailure $ show e
  Nothing -> pure ()
  where
    err = (\(_, merr, _) -> merr) $ Trace.runEmulatorTrace cfg trace
