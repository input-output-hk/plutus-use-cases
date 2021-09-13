module Capability.Navigate where

import Prelude
import Data.Route
import Halogen

class
  Monad m <= Navigate m where
  navigate :: Route -> m Unit

instance navigateHalogenM :: Navigate m => Navigate (HalogenM state action slots msg m) where
  navigate = lift <<< navigate
