module Utils.RemoteDataState where

import Halogen.HTML as HH
import Network.RemoteData (RemoteData(..))

type RemoteDataHandle props act e a = {
  onNotAsked :: HH.HTML props act,
  onLoading :: HH.HTML props act,
  onFailure :: e -> HH.HTML props act,
  onSuccess :: a -> HH.HTML props act
}

makeRemoteDataState :: forall props act e a. RemoteDataHandle props act e a -> RemoteData e a -> HH.HTML props act
makeRemoteDataState { onNotAsked } NotAsked = onNotAsked
makeRemoteDataState { onLoading } Loading = onLoading
makeRemoteDataState { onFailure } (Failure e) = onFailure e
makeRemoteDataState { onSuccess } (Success s) = onSuccess s
