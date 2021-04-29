module Main where

import Test.Tasty
import qualified Test.Lending as Lending

main :: IO ()
main = defaultMain Lending.tests

