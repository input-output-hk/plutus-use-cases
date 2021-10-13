module Test.NFT.Script.Dealing (
  testDealing,
) where

import PlutusTx.Prelude hiding ((<>))
import Test.Tasty (TestTree)

import Data.Semigroup ((<>))
import Ledger qualified
import Mlabs.NFT.Types qualified as NFT
import Mlabs.NFT.Validation qualified as NFT
import Plutus.V1.Ledger.Ada qualified as Ada
import PlutusTx qualified
import PlutusTx.IsData.Class (ToData (toBuiltinData))
import PlutusTx.Prelude hiding ((<>))
import PlutusTx.Prelude qualified as PlutusPrelude
import Test.NFT.Script.Values as TestValues
import Test.Tasty (TestTree, localOption)
import Test.Tasty.Plutus.Context
import Test.Tasty.Plutus.Script.Unit

testDealing :: TestTree
testDealing = withValidator "Test NFT dealing validator" dealingValidator $ do
  shouldValidate "Can buy with ADA" validBuyData validBuyContext
  shouldValidate "Author can set price when owner" validSetPriceData validSetPriceContext
  shouldValidate "Owner can set price" ownerUserOneSetPriceData ownerUserOneSetPriceContext
  shouldn'tValidate "Author can't set price when not owner" ownerUserOneSetPriceData authorNotOwnerSetPriceContext
  shouldn'tValidate "Can't buy if not for sale" notForSaleData notForSaleContext
  shouldn'tValidate "Can't buy if bid not high enough" bidNotHighEnoughData validBuyContext
  shouldn'tValidate "Can't buy if author not paid" validBuyData authorNotPaidContext
  shouldn'tValidate "Can't buy if owner not paid" ownerNotPaidData ownerNotPaidContext
  shouldn'tValidate "Can't buy with inconsistent daatum" inconsistentDatumData inconsistentDatumContext

initialAuthorDatum :: NFT.DatumNft
initialAuthorDatum =
  NFT.DatumNft
    { dNft'id = TestValues.testNftId
    , dNft'share = 1 % 2
    , dNft'author = NFT.UserId $ TestValues.authorPkh
    , dNft'owner = NFT.UserId $ TestValues.authorPkh
    , dNft'price = Just 100
    }

ownerUserOneDatum :: NFT.DatumNft
ownerUserOneDatum = initialAuthorDatum {NFT.dNft'owner = NFT.UserId $ TestValues.userOnePkh}

notForSaleDatum :: NFT.DatumNft
notForSaleDatum = initialAuthorDatum {NFT.dNft'price = Nothing}

ownerNotPaidDatum :: NFT.DatumNft
ownerNotPaidDatum = ownerUserOneDatum

inconsistentDatum :: NFT.DatumNft
inconsistentDatum = initialAuthorDatum {NFT.dNft'share = 1 % 10}

-- Buy test cases

validBuyData :: TestData 'ForSpending
validBuyData = SpendingTest dtm redeemer val
  where
    dtm = initialAuthorDatum

    redeemer =
      NFT.BuyAct
        { act'bid = 100
        , act'newPrice = Nothing
        , act'cs = TestValues.nftCurrencySymbol
        }
    val = TestValues.adaValue 100 <> TestValues.oneNft

notForSaleData :: TestData 'ForSpending
notForSaleData = SpendingTest dtm redeemer val
  where
    dtm = notForSaleDatum

    redeemer =
      NFT.BuyAct
        { act'bid = 100
        , act'newPrice = Just 150
        , act'cs = TestValues.nftCurrencySymbol
        }
    val = TestValues.adaValue 100 <> TestValues.oneNft

bidNotHighEnoughData :: TestData 'ForSpending
bidNotHighEnoughData = SpendingTest dtm redeemer val
  where
    dtm = initialAuthorDatum

    redeemer =
      NFT.BuyAct
        { act'bid = 90
        , act'newPrice = Nothing
        , act'cs = TestValues.nftCurrencySymbol
        }
    val = TestValues.adaValue 100 <> TestValues.oneNft

ownerNotPaidData :: TestData 'ForSpending
ownerNotPaidData = SpendingTest dtm redeemer val
  where
    dtm = ownerNotPaidDatum

    redeemer =
      NFT.BuyAct
        { act'bid = 100
        , act'newPrice = Nothing
        , act'cs = TestValues.nftCurrencySymbol
        }
    val = TestValues.adaValue 0 <> TestValues.oneNft

validBuyContext :: ContextBuilder 'ForSpending
validBuyContext =
  (addDatum initialAuthorDatum)
    <> (paysToWallet TestValues.userOneWallet TestValues.oneNft)
    <> (paysToWallet TestValues.authorWallet (TestValues.adaValue 100))
    <> (paysSelf oneNft initialAuthorDatum)

notForSaleContext :: ContextBuilder 'ForSpending
notForSaleContext =
  (addDatum notForSaleDatum)
    <> (paysToWallet TestValues.userOneWallet TestValues.oneNft)
    <> (paysToWallet TestValues.authorWallet (TestValues.adaValue 100))
    <> (paysSelf oneNft notForSaleDatum)

authorNotPaidContext :: ContextBuilder 'ForSpending
authorNotPaidContext =
  (addDatum initialAuthorDatum)
    <> (paysToWallet TestValues.userOneWallet TestValues.oneNft)
    <> (paysToWallet TestValues.authorWallet (TestValues.adaValue 10))
    <> (paysSelf oneNft initialAuthorDatum)

ownerNotPaidContext :: ContextBuilder 'ForSpending
ownerNotPaidContext =
  (addDatum ownerNotPaidDatum)
    <> (paysToWallet TestValues.userTwoWallet TestValues.oneNft)
    <> (paysToWallet TestValues.authorWallet (TestValues.adaValue 50))
    -- <> (paysToWallet TestValues.userOneWallet (TestValues.adaValue 50))
    <> (paysSelf oneNft ownerNotPaidDatum)

-- SetPrice test cases

validSetPriceData :: TestData 'ForSpending
validSetPriceData = SpendingTest dtm redeemer val
  where
    dtm = initialAuthorDatum

    redeemer =
      NFT.SetPriceAct
        { act'newPrice = Just 150
        , act'cs = TestValues.nftCurrencySymbol
        }
    val = TestValues.oneNft

ownerUserOneSetPriceData :: TestData 'ForSpending
ownerUserOneSetPriceData = SpendingTest dtm redeemer val
  where
    dtm = ownerUserOneDatum

    redeemer =
      NFT.SetPriceAct
        { act'newPrice = Nothing
        , act'cs = TestValues.nftCurrencySymbol
        }
    val = TestValues.oneNft

validSetPriceContext :: ContextBuilder 'ForSpending
validSetPriceContext =
  (addDatum initialAuthorDatum)
    <> signedWith authorPkh
    -- TODO: choose
    <> (paysSelf oneNft initialAuthorDatum)

-- <> (output $ Output (OwnType $ toBuiltinData initialAuthorDatum) TestValues.oneNft)

ownerUserOneSetPriceContext :: ContextBuilder 'ForSpending
ownerUserOneSetPriceContext =
  (addDatum ownerUserOneDatum)
    <> signedWith userOnePkh
    <> (paysSelf oneNft ownerUserOneDatum)

authorNotOwnerSetPriceContext :: ContextBuilder 'ForSpending
authorNotOwnerSetPriceContext =
  (addDatum ownerUserOneDatum)
    <> signedWith authorPkh
    <> (paysSelf oneNft ownerUserOneDatum)

dealingValidator :: Ledger.Validator
dealingValidator =
  Ledger.mkValidatorScript $
    $$(PlutusTx.compile [||wrap||])
      `PlutusTx.applyCode` $$(PlutusTx.compile [||NFT.mkTxPolicy||])
  where
    wrap ::
      (NFT.DatumNft -> NFT.UserAct -> Ledger.ScriptContext -> Bool) ->
      (BuiltinData -> BuiltinData -> BuiltinData -> ())
    wrap = toTestValidator
