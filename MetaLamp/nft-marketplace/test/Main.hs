module Main
  ( main
  ) where

import qualified Abstract.Percentage        as Percentage
import qualified Abstract.RemoteDataSpec    as RemoteData
import qualified Marketplace.Spec.CreateNft as CreateNft
import qualified Marketplace.Spec.Sale      as Sale
import qualified Marketplace.Spec.Start     as Start
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "All tests"
    [ testGroup
        "NFT Marketplace"
        [Start.tests, CreateNft.tests, Sale.tests]
    , testGroup "Abstract" [RemoteData.tests, Percentage.tests]
    ]
