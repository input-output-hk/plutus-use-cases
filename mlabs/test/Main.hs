module Main where

import Test.Tasty

import qualified Test.Lending.Logic as Logic

main :: IO ()
main = defaultMain $ Logic.test

