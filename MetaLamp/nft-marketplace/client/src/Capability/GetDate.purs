module Capability.GetDate where

import Prelude
import Halogen (HalogenM, lift)
import Data.JSDate (JSDate)
import Data.DateTime.Instant (Instant)

class
  Monad m <= GetDate m where
  now :: m Instant
  parseDate :: String -> m JSDate

instance getDateHalogenM :: GetDate m => GetDate (HalogenM st act slots msg m) where
  now = lift now
  parseDate = parseDate >>> lift
