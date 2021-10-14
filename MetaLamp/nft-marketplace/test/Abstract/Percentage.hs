{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Abstract.Percentage
  ( tests
  ) where

import           Plutus.Abstract.Percentage
import           Prelude
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "mkPercentage"
  [ testCase "should make Percentage" $
    (mkPercentage (5, 2)) @?= (Just $ Percentage (5, 2))
  , testCase "should return Nothing if denominator is 0" $
      (mkPercentage (5, 0)) @?= Nothing
  , testCase "should return Nothing if the value is gte 100%" $
      (mkPercentage (400, 2)) @?= Nothing
  ]
