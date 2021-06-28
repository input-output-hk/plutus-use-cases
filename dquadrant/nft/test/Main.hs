
import  qualified Test.DirectSaleEndpointTest
import  qualified Test.AuctionEndpointTest

import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "marketplacce" [
        Test.DirectSaleEndpointTest.tests,
        Test.AuctionEndpointTest.tests
    ]