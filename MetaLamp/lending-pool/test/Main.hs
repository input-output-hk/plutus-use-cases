module Main(main) where

import qualified Spec.Start
import qualified Spec.User
import           Test.Tasty
import           Test.Tasty.Hedgehog (HedgehogTestLimit (..))

main :: IO ()
main = defaultMain tests

limit :: HedgehogTestLimit
limit = HedgehogTestLimit (Just 5)

tests :: TestTree
tests = localOption limit $ testGroup "lending pool tests" [
    Spec.Start.tests,
    Spec.User.tests
    ]
