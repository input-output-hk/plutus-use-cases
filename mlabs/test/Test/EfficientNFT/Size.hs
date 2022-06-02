module Test.EfficientNFT.Size (test) where

import Plutus.V1.Ledger.Scripts (fromCompiledCode)
import PlutusTx qualified
import PlutusTx.Prelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Plutus.Script.Size (fitsOnChain)

import Mlabs.EfficientNFT.Dao qualified as Dao
import Mlabs.EfficientNFT.Lock qualified as Lock
import Mlabs.EfficientNFT.Marketplace qualified as Marketplace
import Mlabs.EfficientNFT.Token (mkPolicy)

test :: TestTree
test =
  testGroup
    "Size"
    [ testMintingPolicyFitOnChain
    , testLockScriptFitOnChain
    , testMarketplaceScriptFitOnChain
    , testDaoScriptFitOnChain
    ]

testMintingPolicyFitOnChain :: TestTree
testMintingPolicyFitOnChain =
  fitsOnChain "Minting policy" $
    fromCompiledCode $$(PlutusTx.compile [||mkPolicy||])

testLockScriptFitOnChain :: TestTree
testLockScriptFitOnChain =
  fitsOnChain "Lock script" $
    fromCompiledCode $$(PlutusTx.compile [||Lock.mkValidator||])

testMarketplaceScriptFitOnChain :: TestTree
testMarketplaceScriptFitOnChain =
  fitsOnChain "Marketplace script" $
    fromCompiledCode $$(PlutusTx.compile [||Marketplace.mkValidator||])

testDaoScriptFitOnChain :: TestTree
testDaoScriptFitOnChain =
  fitsOnChain "Dao script" $
    fromCompiledCode $$(PlutusTx.compile [||Dao.mkValidator||])
