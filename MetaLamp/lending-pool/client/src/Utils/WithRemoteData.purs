module Utils.WithRemoteData where

import Prelude
import Data.Lens (Lens', set)
import Halogen as H
import Network.RemoteData (RemoteData(..))

withRemoteData ::
  forall e a s action slots output m.
  (Lens' s (RemoteData e a)) ->
  H.HalogenM s action slots output m (RemoteData e a) ->
  H.HalogenM s action slots output m Unit
withRemoteData =
  withRemoteData'
    $ { before: pure unit, after: (const <<< pure $ unit) }

withRemoteData' ::
  forall e a s action slots output m.
  { before :: H.HalogenM s action slots output m Unit
  , after :: RemoteData e a -> H.HalogenM s action slots output m Unit
  } ->
  (Lens' s (RemoteData e a)) ->
  H.HalogenM s action slots output m (RemoteData e a) ->
  H.HalogenM s action slots output m Unit
withRemoteData' { before, after } l action = do
  state <- H.get
  H.put $ set l Loading state
  before
  result <- action
  H.put $ set l result state
  after result
