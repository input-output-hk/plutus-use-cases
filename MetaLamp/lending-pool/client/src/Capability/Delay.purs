module Capability.Delay where

import Prelude
import Data.Time.Duration (Milliseconds)
import Halogen (HalogenM, lift)

class
  Monad m <= Delay m where
  delay :: Milliseconds -> m Unit

instance delayHalogenM :: Delay m => Delay (HalogenM st act slots msg m) where
  delay = delay >>> lift
