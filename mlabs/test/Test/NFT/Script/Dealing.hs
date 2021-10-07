module Test.NFT.Script.Dealing
  ( testDealing
  ) where

import Test.Tasty (TestTree)
import PlutusTx.Prelude hiding ((<>))

import qualified Ledger
import qualified Mlabs.NFT.Validation          as NFT
import           PlutusTx.Prelude hiding ((<>))
import qualified PlutusTx.Prelude as PlutusPrelude
import           Test.NFT.Script.Values               as TestValues
import           Test.Tasty                    (TestTree, localOption)
import           Test.Tasty.Plutus.Context
import           Test.Tasty.Plutus.Script.Unit
import qualified PlutusTx
import Data.Semigroup ((<>))
import qualified Plutus.V1.Ledger.Ada as Ada

testDealing :: TestTree
testDealing = withValidator "Test NFT dealing validator" dealingValidator $ do
  shouldValidate "Can buy with ADA" validBuyData validBuyContext

validBuyData :: TestData 'ForSpending
validBuyData = SpendingTest datum redeemer val
  where
    datum = NFT.DatumNft { dNft'id = TestValues.testNftId
                         , dNft'share = 1 % 2
                         , dNft'author = error ()
                         , dNft'owner = error ()
                         , dNft'price = Just 100
                         }

    redeemer = NFT.BuyAct { act'bid = 100
                          , act'newPrice = Nothing
                          , act'cs = Ada.adaSymbol
                          }
    val = error ()

validBuyContext :: ContextBuilder 'ForSpending
validBuyContext = error ()

dealingValidator :: Ledger.Validator
dealingValidator =
  Ledger.mkValidatorScript $
    $$(PlutusTx.compile [||wrap||])
    `PlutusTx.applyCode` $$(PlutusTx.compile [||NFT.mkTxPolicy||])
  where
    wrap :: (NFT.DatumNft -> NFT.UserAct -> Ledger.ScriptContext -> Bool) ->
           (BuiltinData -> BuiltinData -> BuiltinData -> ())
    wrap = toTestValidator
