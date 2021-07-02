
import  qualified Test.Wallet.DirectSaleEndpointTest
import  qualified Test.Wallet.AuctionEndpointTest

import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "marketplacce" [
        Test.Wallet.DirectSaleEndpointTest.tests,
        Test.Wallet.AuctionEndpointTest.tests
    ]