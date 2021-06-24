module Main where

import Prelude

import App.Button as Button
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import PAB.Api
import PAB.Types 

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Button.component unit body
