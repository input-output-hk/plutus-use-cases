module Main where

import Prelude

import AppComponent as B
import AppAff (runAppM)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main =
  runHalogenAff do
    let rootComponent = H.hoist (runAppM { host: "localhost", port: 8080 }) B.component
    body <- awaitBody
    runUI rootComponent unit body

onLoad :: Unit
onLoad = unsafePerformEffect main
