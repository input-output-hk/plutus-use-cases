module Utils.BEM where

import Prelude
import Halogen (ClassName(..))

block :: String -> String -> ClassName
block base next = case next of
  "" -> ClassName base
  s -> ClassName $ base <> "__" <> s
