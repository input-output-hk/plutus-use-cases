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
import Routing.Duplex as Routing
import Routing.Hash as Routing
import Effect.Aff (Aff, launchAff_)

-- TODO add to readme
-- ipfs config --json API.HTTPHeaders.Access-Control-Allow-Origin '["webui://-", "http://localhost:3000", "http://127.0.0.1:5001", "https://webui.ipfs.io", "https://localhost:8009", "http://localhost:8009"]'
-- ipfs config --json API.HTTPHeaders.Access-Control-Allow-Methods '["PUT", "POST"]'
main :: Effect Unit
main =
  runHalogenAff do
    let
      env = { ipfsServer: { host: "localhost", port: 5001 }, pabServer: { host: "localhost", port: 8080 } }
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
