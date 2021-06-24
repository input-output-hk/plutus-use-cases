
import  qualified Test.MarketEndpointTest
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "stablecoin" [
        Test.MarketEndpointTest.tests
    ]