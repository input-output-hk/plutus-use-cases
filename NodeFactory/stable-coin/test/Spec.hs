module Main
    ( main
    ) where

import qualified Spec.Trace

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "token sale"
    [ Spec.Trace.tests ]