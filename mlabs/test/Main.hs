module Main where

import Test.Tasty

import qualified Test.Lending.Logic as Logic
import qualified Test.Lending as Lending

main :: IO ()
main = defaultMain $ testGroup "Lending"
  [ Logic.test
  , Lending.tests
  ]

