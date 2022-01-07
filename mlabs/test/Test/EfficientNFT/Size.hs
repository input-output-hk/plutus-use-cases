module Test.NFT.Size (test) where

import Plutus.V1.Ledger.Scripts (Script, fromCompiledCode)
import PlutusTx qualified
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Plutus.Script.Size (fitsOnChain)
import Prelude (String)

import Mlabs.EfficientNFT.Token (mkPolicy)

test :: TestTree
test = testGroup "Size" [testFitOnChain]

testFitOnChain :: TestTree
testFitOnChain = fitsOnChain scriptName script

scriptName :: String
scriptName = "NFT marketplace"

script :: Script
script = fromCompiledCode $$(PlutusTx.compile [||mkPolicy||]) 
