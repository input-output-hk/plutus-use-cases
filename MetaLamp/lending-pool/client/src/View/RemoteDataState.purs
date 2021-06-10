module View.RemoteDataState where

import Prelude
import Halogen.HTML as HH
import Network.RemoteData (RemoteData(..))

remoteDataState :: forall props act e a. Show e => (a -> HH.HTML props act) -> RemoteData e a -> HH.HTML props act
remoteDataState onSuccess = case _ of
  NotAsked -> HH.div_ [ HH.text "" ]
  Loading -> HH.div_ [ HH.text "Loading..." ]
  Failure e -> HH.div_ [ HH.text $ "Error: " <> show e ]
  Success a -> onSuccess a
