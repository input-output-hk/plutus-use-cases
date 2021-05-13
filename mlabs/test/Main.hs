module Main where

import Test.Tasty

import qualified Test.Lending.Contract as Contract
import qualified Test.Lending.Logic as Logic

main :: IO ()
main = defaultMain $ testGroup "Lending"
  [ Logic.test
  , Contract.test
  ]

