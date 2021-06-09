module View.RemoteDataState where

import Prelude
import Halogen.HTML as HH
import Network.RemoteData (RemoteData)
import Utils.RemoteDataState (makeRemoteDataState)

remoteDataState :: forall props act e a. Show e => (a -> HH.HTML props act) -> RemoteData e a -> HH.HTML props act
remoteDataState onSuccess =
  makeRemoteDataState
    { onNotAsked: HH.div_ [ HH.text "" ]
    , onLoading: HH.div_ [ HH.text "Loading..." ]
    , onFailure: \e -> HH.div_ [ HH.text $ "Error: " <> show e ]
    , onSuccess
    }
