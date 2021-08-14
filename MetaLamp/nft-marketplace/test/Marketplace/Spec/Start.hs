{-# LANGUAGE FlexibleContexts #-}

module Marketplace.Spec.Start
  ( tests
  ) where

import Plutus.Contract.Test
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "start"
    [ checkPredicateOptions
        options
        "Should start a new marketplace with empty store"
        datumsCheck
        startTrace
    ]
