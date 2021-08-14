module Main
    ( main
    ) where

import qualified Marketplace.Spec.Start as Start
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "NFT Marketplace"
    [ Start.tests
    ]
