{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import qualified Spec.StableCoinTest
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "stablecoin" [
    Spec.StableCoinTest.tests
    ]
