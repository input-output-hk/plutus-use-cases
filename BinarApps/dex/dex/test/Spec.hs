module Main (main) where

import           Test.Tasty

import qualified Spec.UseCases

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "dex" [ Spec.UseCases.tests ]
