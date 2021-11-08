module Test.NFT.Script.Main where

import Test.NFT.Script.Dealing
import Test.NFT.Script.Minting
import Test.NFT.Script.Auction
import Test.Tasty (TestTree, testGroup)

test :: TestTree
test =
  testGroup
    "Script"
    [ testMinting
    , testDealing
    , testAuctionBeforeDeadline
    , testAuctionAfterDeadline
    ]
