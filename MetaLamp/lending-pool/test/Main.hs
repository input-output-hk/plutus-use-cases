module Main(main) where

import qualified Spec.Borrow
import qualified Spec.Deposit
import qualified Spec.ProvideCollateral
import qualified Spec.Repay
import qualified Spec.RevokeCollateral
import qualified Spec.Start
import qualified Spec.Withdraw
import           Test.Tasty
import           Test.Tasty.Hedgehog    (HedgehogTestLimit (..))

main :: IO ()
main = defaultMain tests

limit :: HedgehogTestLimit
limit = HedgehogTestLimit (Just 5)

tests :: TestTree
tests = localOption limit $ testGroup "lending pool tests" [
    Spec.Start.tests,
    Spec.Deposit.tests,
    Spec.Withdraw.tests,
    Spec.ProvideCollateral.tests,
    Spec.RevokeCollateral.tests,
    Spec.Borrow.tests,
    Spec.Repay.tests
    ]
