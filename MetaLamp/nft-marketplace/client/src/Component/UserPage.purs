module Component.UserPage where

import Prelude
import Data.Symbol (SProxy(..))
import Halogen (Component)
import Halogen as H
import Halogen.HTML as HH

type Slot id
  = forall query. H.Slot query Void id

_userPage :: SProxy "userPage"
_userPage = SProxy

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval
    }

render :: forall state action m. state -> H.ComponentHTML action () m
render _ = HH.h1_ [ HH.text "UserPage" ]
