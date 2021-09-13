module Main where

import Prelude
import Data.Maybe (Maybe(..))
import AppAff (runAppM)
import Component.MainPage as App
import Data.Route (routeCodec)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse) as Routing
import Routing.Hash (matchesWith) as Routing
import Effect.Aff (launchAff_)

main :: Effect Unit
main =
  runHalogenAff do
    let
      env = { ipfsServer: { host: "localhost", port: 5001 }, pabServer: { host: "localhost", port: 9080 } }
    let
      rootComponent = H.hoist (runAppM env) App.component
    body <- awaitBody
    halogenIO <- runUI rootComponent unit body
    void $ liftEffect
      $ Routing.matchesWith (Routing.parse routeCodec) \oldRoute newRoute ->
          when (oldRoute /= Just newRoute) do
            launchAff_ $ halogenIO.query $ H.tell $ App.NavigateTo newRoute
    pure unit

onLoad :: Unit
onLoad = unsafePerformEffect main
