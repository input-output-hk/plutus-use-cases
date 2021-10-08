module Test.NFT.Script.Dealing
  ( testDealing
  ) where

import Test.Tasty (TestTree)
import PlutusTx.Prelude hiding ((<>))

import qualified Mlabs.NFT.Types as NFT
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
import PlutusTx.IsData.Class (ToData (toBuiltinData))

testDealing :: TestTree
testDealing = withValidator "Test NFT dealing validator" dealingValidator $ do
  shouldValidate "Can buy with ADA" validBuyData validBuyContext

initialAuthorDatum :: NFT.DatumNft
initialAuthorDatum = NFT.DatumNft { dNft'id = TestValues.testNftId
                         , dNft'share = 1 % 2
                         , dNft'author = NFT.UserId $ TestValues.authorPkh
                         , dNft'owner = NFT.UserId $ TestValues.authorPkh
                         , dNft'price = Just 100
                         }

validBuyData :: TestData 'ForSpending
validBuyData = SpendingTest datum redeemer val
  where
    datum = initialAuthorDatum

    redeemer = NFT.BuyAct { act'bid = 100
                          , act'newPrice = Nothing
                          , act'cs = Ada.adaSymbol
                          }
    val = TestValues.adaValue 100

validBuyContext :: ContextBuilder 'ForSpending
validBuyContext = (addDatum initialAuthorDatum)
  <> (input $ Input (OwnType $ toBuiltinData initialAuthorDatum) TestValues.oneAda)
  <> (paysToWallet TestValues.userOneWallet) TestValues.oneNft

dealingValidator :: Ledger.Validator
dealingValidator =
  Ledger.mkValidatorScript $
    $$(PlutusTx.compile [||wrap||])
    `PlutusTx.applyCode` $$(PlutusTx.compile [||NFT.mkTxPolicy||])
  where
    wrap :: (NFT.DatumNft -> NFT.UserAct -> Ledger.ScriptContext -> Bool) ->
           (BuiltinData -> BuiltinData -> BuiltinData -> ())
    wrap = toTestValidator
