{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import qualified Spec.StableCoinTest
import qualified Spec.OracleTest
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "stablecoin" [
    Spec.StableCoinTest.tests,
    Spec.OracleTest.tests
    ]
