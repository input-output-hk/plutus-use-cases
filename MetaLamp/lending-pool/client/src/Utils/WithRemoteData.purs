module Utils.WithRemoteData where

import Prelude
import Data.Either (Either, either)
import Data.Lens (Lens', over)
import Halogen as H
import Network.RemoteData (RemoteData(..))

runRDWith ::
  forall e a s action slots output m.
  (Lens' s (RemoteData e a)) ->
  H.HalogenM s action slots output m (Either e a) ->
  H.HalogenM s action slots output m (RemoteData e a)
runRDWith l action = do
  H.modify_ $ over l (const Loading)
  result <- either Failure Success <$> action
  H.modify_ $ over l (const result)
  pure result
