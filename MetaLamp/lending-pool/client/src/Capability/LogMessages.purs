module Capability.LogMessages where

import Prelude
import Halogen (HalogenM, lift)

class
  Monad m <= LogMessages m where
  logInfo :: String -> m Unit
  logError :: String -> m Unit

instance logMessagesHalogenM :: LogMessages m => LogMessages (HalogenM st act slots msg m) where
  logInfo = logInfo >>> lift
  logError = logError >>> lift
