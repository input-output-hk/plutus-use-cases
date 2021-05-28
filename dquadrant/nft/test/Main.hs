
import  qualified Test.MarketPlace
    
main :: IO()
main=do
    putStrLn "Starting test"
    Test.MarketPlace.test
    putStrLn "Done test"