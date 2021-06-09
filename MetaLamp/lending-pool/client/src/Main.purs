module Main where

import Prelude
import Components.App as App
import AppAff (runAppM)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main =
  runHalogenAff do
    let
      rootComponent = H.hoist (runAppM { host: "localhost", port: 8080 }) App.component
    body <- awaitBody
    runUI rootComponent unit body

onLoad :: Unit
onLoad = unsafePerformEffect main
