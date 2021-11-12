module Middleware.Capability.ReqIdGen
  ( ReqIdGen
  , nextReqId
  , runReqIdGen
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text              (Text)
import           Data.UUID              (toText)
import           Data.UUID.V4           as UUID (nextRandom)
import           Polysemy


data ReqIdGen m a where
  NextReqId :: ReqIdGen m Text -- todo: change text to ???

makeSem ''ReqIdGen

runReqIdGen
  :: Member (Embed IO) r
  => Sem (ReqIdGen ': r) a
  -> Sem r a
runReqIdGen = interpret $ \case
  NextReqId -> embed $ toText <$> UUID.nextRandom
