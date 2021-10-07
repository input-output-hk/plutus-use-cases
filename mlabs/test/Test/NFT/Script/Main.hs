-- TODO: split & move to other modules
module Test.NFT.Main where

import Test.Tasty (TestTree, testGroup)
import Test.NFT.Minting
import Test.NFT.Dealing

test :: TestTree
test =
  testGroup
    "NFT rewrite script tests"
    [ testMinting
    , testDealing
    ]
