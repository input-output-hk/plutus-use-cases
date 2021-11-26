module Test.NFT.Script.Dealing (
  testDealing,
) where

import Data.Semigroup ((<>))
import Ledger qualified
import Mlabs.NFT.Types qualified as NFT
import Mlabs.NFT.Validation qualified as NFT

import PlutusTx.Prelude hiding ((<>))

import PlutusTx qualified
import Test.NFT.Script.Values as TestValues
import Test.Tasty (TestTree)
import Test.Tasty.Plutus.Context
import Test.Tasty.Plutus.Script.Unit

testDealing :: TestTree
testDealing = withValidator "Test NFT dealing validator" dealingValidator $ do
  shouldValidate "Can buy from author" validBuyData validBuyContext
  shouldValidate "Author can set price when owner" validSetPriceData validSetPriceContext
  shouldValidate "Owner can set price" ownerUserOneSetPriceData ownerUserOneSetPriceContext
  shouldn'tValidate "Author can't set price when not owner" ownerUserOneSetPriceData authorNotOwnerSetPriceContext
  shouldn'tValidate "Can't set price if mismatching id" validSetPriceData mismathingIdSetPriceContext
  shouldn'tValidate "Can't buy if not for sale" notForSaleData notForSaleContext
  shouldn'tValidate "Can't buy if bid not high enough" bidNotHighEnoughData bidNotHighEnoughContext
  -- FIXME #269 will fix this
  -- shouldn'tValidate "Can't buy if author not paid" validBuyData authorNotPaidContext
  -- shouldn'tValidate "Can't buy if owner not paid" ownerNotPaidData ownerNotPaidContext
  shouldn'tValidate "Can't buy if mismatching id" validBuyData mismathingIdBuyContext

-- TODO: bring back this test if `tasty-plutus` would allow to change datum order
-- shouldn'tValidate "Can't buy with inconsistent datum" validBuyData inconsistentDatumContext

initialNode :: NFT.NftListNode
initialNode =
  NFT.NftListNode
    { node'information =
        NFT.InformationNft
          { info'id = TestValues.testNftId
          , info'share = 1 % 2
          , info'author = NFT.UserId TestValues.authorPkh
          , info'owner = NFT.UserId TestValues.authorPkh
          , info'price = Just (100 * 1_000_000)
          , info'auctionState = Nothing
          }
    , node'next = Nothing
    , node'appInstance = TestValues.appInstance
    }

initialAuthorDatum :: NFT.DatumNft
initialAuthorDatum = NFT.NodeDatum initialNode

ownerUserOneDatum :: NFT.DatumNft
ownerUserOneDatum =
  NFT.NodeDatum $
    initialNode
      { NFT.node'information =
          (NFT.node'information initialNode)
            { NFT.info'owner = NFT.UserId TestValues.userOnePkh
            }
      }

notForSaleDatum :: NFT.DatumNft
notForSaleDatum =
  NFT.NodeDatum $
    initialNode
      { NFT.node'information =
          (NFT.node'information initialNode)
            { NFT.info'price = Nothing
            }
      }

ownerNotPaidDatum :: NFT.DatumNft
ownerNotPaidDatum = ownerUserOneDatum

inconsistentDatum :: NFT.DatumNft
inconsistentDatum =
  NFT.NodeDatum $
    initialNode
      { NFT.node'information =
          (NFT.node'information initialNode)
            { NFT.info'share = 1 % 10
            }
      }

-- Buy test cases

validBuyData :: TestData 'ForSpending
validBuyData = SpendingTest dtm redeemer val
  where
    dtm = initialAuthorDatum

    redeemer =
      NFT.BuyAct
        { act'bid = 100 * 1_000_000
        , act'newPrice = Nothing
        , act'symbol = TestValues.appSymbol
        }
    val = TestValues.adaValue 100 <> TestValues.oneNft

notForSaleData :: TestData 'ForSpending
notForSaleData = SpendingTest dtm redeemer val
  where
    dtm = notForSaleDatum

    redeemer =
      NFT.BuyAct
        { act'bid = 100 * 1_000_000
        , act'newPrice = Just 150
        , act'symbol = TestValues.appSymbol
        }
    val = TestValues.adaValue 100 <> TestValues.oneNft

bidNotHighEnoughData :: TestData 'ForSpending
bidNotHighEnoughData = SpendingTest dtm redeemer val
  where
    dtm = initialAuthorDatum

    redeemer =
      NFT.BuyAct
        { act'bid = 90 * 1_000_000
        , act'newPrice = Nothing
        , act'symbol = TestValues.appSymbol
        }
    val = TestValues.adaValue 90 <> TestValues.oneNft

ownerNotPaidData :: TestData 'ForSpending
ownerNotPaidData = SpendingTest dtm redeemer val
  where
    dtm = ownerNotPaidDatum

    redeemer =
      NFT.BuyAct
        { act'bid = 100 * 1_000_000
        , act'newPrice = Nothing
        , act'symbol = TestValues.appSymbol
        }
    val = TestValues.adaValue 0 <> TestValues.oneNft

inconsistentDatumData :: TestData 'ForSpending
inconsistentDatumData = SpendingTest dtm redeemer val
  where
    dtm = initialAuthorDatum

    redeemer =
      NFT.BuyAct
        { act'bid = 100 * 1_000_000
        , act'newPrice = Nothing
        , act'symbol = TestValues.appSymbol
        }
    val = TestValues.adaValue 100 <> TestValues.oneNft

validBuyContext :: ContextBuilder 'ForSpending
validBuyContext =
  paysToWallet TestValues.authorWallet (TestValues.adaValue 100)
    <> paysOther (NFT.txValHash uniqueAsset) oneNft initialAuthorDatum

bidNotHighEnoughContext :: ContextBuilder 'ForSpending
bidNotHighEnoughContext =
  paysToWallet TestValues.authorWallet (TestValues.adaValue 90)
    <> paysOther (NFT.txValHash uniqueAsset) oneNft initialAuthorDatum

notForSaleContext :: ContextBuilder 'ForSpending
notForSaleContext =
  paysToWallet TestValues.userOneWallet TestValues.oneNft
    <> paysToWallet TestValues.authorWallet (TestValues.adaValue 100)
    <> paysOther (NFT.txValHash uniqueAsset) oneNft notForSaleDatum

authorNotPaidContext :: ContextBuilder 'ForSpending
authorNotPaidContext =
  paysToWallet TestValues.authorWallet (TestValues.adaValue 5)
    <> paysOther (NFT.txValHash uniqueAsset) oneNft initialAuthorDatum

ownerNotPaidContext :: ContextBuilder 'ForSpending
ownerNotPaidContext =
  paysToWallet TestValues.authorWallet (TestValues.adaValue 50)
    <> paysOther (NFT.txValHash uniqueAsset) oneNft ownerNotPaidDatum

inconsistentDatumContext :: ContextBuilder 'ForSpending
inconsistentDatumContext =
  paysToWallet TestValues.userOneWallet TestValues.oneNft
    <> paysToWallet TestValues.authorWallet (TestValues.adaValue 100)
    <> paysOther (NFT.txValHash uniqueAsset) oneNft inconsistentDatum

mismathingIdBuyContext :: ContextBuilder 'ForSpending
mismathingIdBuyContext =
  paysToWallet TestValues.authorWallet (TestValues.adaValue 100)
    <> paysOther (NFT.txValHash uniqueAsset) oneNft dtm
  where
    dtm =
      NFT.NodeDatum $
        initialNode
          { NFT.node'information = ((NFT.node'information initialNode) {NFT.info'id = NFT.NftId "I AM INVALID"})
          }

-- SetPrice test cases

validSetPriceData :: TestData 'ForSpending
validSetPriceData = SpendingTest dtm redeemer val
  where
    dtm = initialAuthorDatum

    redeemer =
      NFT.SetPriceAct
        { act'newPrice = Just (150 * 1_000_000)
        , act'symbol = TestValues.appSymbol
        }
    val = TestValues.oneNft

ownerUserOneSetPriceData :: TestData 'ForSpending
ownerUserOneSetPriceData = SpendingTest dtm redeemer val
  where
    dtm = ownerUserOneDatum

    redeemer =
      NFT.SetPriceAct
        { act'newPrice = Nothing
        , act'symbol = TestValues.appSymbol
        }
    val = TestValues.oneNft

validSetPriceContext :: ContextBuilder 'ForSpending
validSetPriceContext =
  signedWith authorPkh
    -- TODO: choose between `paysOther NFT.txValHash` and `output` (see below)
    <> paysOther (NFT.txValHash uniqueAsset) oneNft initialAuthorDatum

-- <> (output $ Output (OwnType $ toBuiltinData initialAuthorDatum) TestValues.oneNft)

ownerUserOneSetPriceContext :: ContextBuilder 'ForSpending
ownerUserOneSetPriceContext =
  signedWith userOnePkh
    <> paysOther (NFT.txValHash uniqueAsset) oneNft ownerUserOneDatum

authorNotOwnerSetPriceContext :: ContextBuilder 'ForSpending
authorNotOwnerSetPriceContext =
  signedWith authorPkh
    <> paysOther (NFT.txValHash uniqueAsset) oneNft ownerUserOneDatum

mismathingIdSetPriceContext :: ContextBuilder 'ForSpending
mismathingIdSetPriceContext =
  signedWith authorPkh
    <> paysOther (NFT.txValHash uniqueAsset) oneNft dtm
  where
    dtm =
      NFT.NodeDatum $
        initialNode
          { NFT.node'information = ((NFT.node'information initialNode) {NFT.info'id = NFT.NftId "I AM INVALID"})
          }

-- todo: fix parametrisation/hard-coding
dealingValidator :: Ledger.Validator
dealingValidator =
  Ledger.mkValidatorScript $
    $$(PlutusTx.compile [||wrap||])
      `PlutusTx.applyCode` ($$(PlutusTx.compile [||NFT.mkTxPolicy||]) `PlutusTx.applyCode` PlutusTx.liftCode uniqueAsset)
  where
    wrap ::
      (NFT.DatumNft -> NFT.UserAct -> Ledger.ScriptContext -> Bool) ->
      (BuiltinData -> BuiltinData -> BuiltinData -> ())
    wrap = toTestValidator
