{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main(main) where

import Spec.MutualBet qualified
import Spec.MutualBetSM qualified
import Spec.Oracle qualified
import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (..))

main :: IO ()
main = defaultMain tests

-- | Number of successful tests for each hedgehog property.
--   The default is 100 but we use a smaller number here in order to speed up
--   the test suite.
--
limit :: HedgehogTestLimit
limit = HedgehogTestLimit (Just 5)

tests :: TestTree
tests = localOption limit $ testGroup "use cases" [
    Spec.MutualBet.tests
    ,
    Spec.MutualBetSM.tests
    ,
    Spec.Oracle.tests
    ]
