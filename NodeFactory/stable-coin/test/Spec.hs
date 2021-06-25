module Main
    ( main
    ) where

import qualified Spec.Trace

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "stable coin"
    [ Spec.Trace.tests ]