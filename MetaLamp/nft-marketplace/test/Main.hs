module Main
  ( main
  ) where

import qualified Abstract.RemoteDataSpec    as RemoteData
import qualified Marketplace.Spec.Auction   as Auction
import qualified Marketplace.Spec.Bundles   as Bundles
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
        [Start.tests, CreateNft.tests, Bundles.tests, Sale.tests, Auction.tests]
    , testGroup "Abstract" [RemoteData.tests]
    ]
