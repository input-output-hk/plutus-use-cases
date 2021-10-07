module Test.NFT.Script.Main where

import Test.Tasty (TestTree, testGroup)
import Test.NFT.Script.Minting
import Test.NFT.Script.Dealing

test :: TestTree
test =
  testGroup
    "NFT rewrite script tests"
    [ testMinting
    , testDealing
    ]
