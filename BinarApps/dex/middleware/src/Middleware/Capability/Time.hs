module Middleware.Capability.Time where


import Control.Concurrent     (threadDelay)
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Polysemy

data Time m r where
  Sleep :: Integer -> Time m ()

makeSem ''Time

runTime :: Member (Embed IO) r => Sem (Time ': r) a -> Sem r a
runTime =
  interpret $ \case (Sleep delay) -> when (delay > 0) $ embed $ threadDelay (fromIntegral delay)
