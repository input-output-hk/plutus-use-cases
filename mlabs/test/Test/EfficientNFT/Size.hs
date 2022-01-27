module Test.EfficientNFT.Size (test) where

import Plutus.V1.Ledger.Scripts (fromCompiledCode)
import PlutusTx qualified
import PlutusTx.Prelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Plutus.Script.Size (fitsOnChain)

import Mlabs.EfficientNFT.Lock (mkValidator)
import Mlabs.EfficientNFT.Token (mkPolicy)

test :: TestTree
test =
  testGroup
    "Size"
    [ testMintingPolicyFitOnChain
    , testLockScriptFitOnChain
    ]

testMintingPolicyFitOnChain :: TestTree
testMintingPolicyFitOnChain =
  fitsOnChain "Minting policy" $
    fromCompiledCode $$(PlutusTx.compile [||mkPolicy||])

testLockScriptFitOnChain :: TestTree
testLockScriptFitOnChain =
  fitsOnChain "Lock script" $
    fromCompiledCode $$(PlutusTx.compile [||mkValidator||])
