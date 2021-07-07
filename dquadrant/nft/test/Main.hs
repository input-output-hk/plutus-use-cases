
import  qualified Test.Wallet.DirectSaleEndpointTest
import  qualified Test.Wallet.AuctionEndpointTest
import qualified Test.Validator.MarketValidatorTest

import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "marketplacce" [
         Test.Validator.MarketValidatorTest.tests,
         Test.Wallet.DirectSaleEndpointTest.tests,
         Test.Wallet.AuctionEndpointTest.tests
    ]
