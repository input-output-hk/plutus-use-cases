module Test.NFT.Script.Dealing (
  testDealing,
) where

import PlutusTx qualified
import PlutusTx.Prelude hiding ((<>))

import Data.Semigroup ((<>))

import Ledger qualified
import Test.NFT.Script.Values as TestValues
import Test.Tasty (TestTree)
import Test.Tasty.Plutus.Context (
  ContextBuilder,
  Purpose (ForSpending),
  paysToOther,
  paysToWallet,
  signedWith,
 )
import Test.Tasty.Plutus.Script.Unit (
  shouldValidate,
  shouldn'tValidate,
 )
import Test.Tasty.Plutus.WithScript

import Ledger (unPaymentPubKeyHash)
import Ledger.Typed.Scripts (Any, TypedValidator, unsafeMkTypedValidator)
import Mlabs.NFT.Spooky (toSpooky)
import Mlabs.NFT.Types qualified as NFT
import Mlabs.NFT.Validation qualified as NFT
import PlutusTx.IsData (ToData (toBuiltinData))
import Test.Tasty.Plutus.TestData

testDealing :: TestTree
testDealing = withValidator "Test NFT dealing validator" dealingValidator $ do
  shouldValidate "Can buy from author" validBuyData validBuyContext
  shouldValidate "Author can set price when owner" validSetPriceData validSetPriceContext
  shouldValidate "Owner can set price" ownerUserOneSetPriceData ownerUserOneSetPriceContext
  shouldn'tValidate "Author can't set price when not owner" ownerUserOneSetPriceData authorNotOwnerSetPriceContext
  shouldn'tValidate "Can't set price if mismatching id" validSetPriceData mismathingIdSetPriceContext
  shouldn'tValidate "Can't buy if not for sale" notForSaleData notForSaleContext
  shouldn'tValidate "Can't buy if bid not high enough" bidNotHighEnoughData bidNotHighEnoughContext
  shouldn'tValidate "Can't buy if author not paid" validBuyData authorNotPaidContext
  shouldn'tValidate "Can't buy if owner not paid" ownerNotPaidData ownerNotPaidContext
  shouldn'tValidate "Can't buy if mismatching id" validBuyData mismathingIdBuyContext

-- TODO: bring back this test if `tasty-plutus` would allow to change datum order
-- shouldn'tValidate "Can't buy with inconsistent datum" validBuyData inconsistentDatumContext

initialNode :: NFT.NftListNode
initialNode =
  NFT.NftListNode
    { node'information' =
        toSpooky $
          NFT.InformationNft
            { info'id' = toSpooky TestValues.testNftId
            , info'share' = toSpooky (1 % 2)
            , info'author' = toSpooky . NFT.UserId . toSpooky $ TestValues.authorPkh
            , info'owner' = toSpooky . NFT.UserId . toSpooky $ TestValues.authorPkh
            , info'price' = toSpooky @(Maybe Integer) $ Just (100 * 1_000_000)
            , info'auctionState' = toSpooky @(Maybe NFT.AuctionState) Nothing
            }
    , node'next' = toSpooky @(Maybe NFT.Pointer) Nothing
    , node'appInstance' = toSpooky TestValues.appInstance
    }

initialAuthorDatum :: NFT.DatumNft
initialAuthorDatum = NFT.NodeDatum initialNode

ownerUserOneDatum :: NFT.DatumNft
ownerUserOneDatum =
  NFT.NodeDatum $
    initialNode
      { NFT.node'information' =
          toSpooky $
            (NFT.node'information initialNode)
              { NFT.info'owner' = toSpooky . NFT.UserId . toSpooky $ TestValues.userOnePkh
              }
      }

notForSaleDatum :: NFT.DatumNft
notForSaleDatum =
  NFT.NodeDatum $
    initialNode
      { NFT.node'information' =
          toSpooky $
            (NFT.node'information initialNode)
              { NFT.info'price' = toSpooky @(Maybe Integer) Nothing
              }
      }

ownerNotPaidDatum :: NFT.DatumNft
ownerNotPaidDatum = ownerUserOneDatum

inconsistentDatum :: NFT.DatumNft
inconsistentDatum =
  NFT.NodeDatum $
    initialNode
      { NFT.node'information' =
          toSpooky $
            (NFT.node'information initialNode)
              { NFT.info'share' = toSpooky (1 % 10)
              }
      }

-- Buy test cases

validBuyData :: TestData ( 'ForSpending BuiltinData BuiltinData)
validBuyData = SpendingTest dtm redeemer val
  where
    dtm = toBuiltinData initialAuthorDatum

    redeemer =
      toBuiltinData $
        NFT.BuyAct
          { act'bid' = toSpooky @Integer (100 * 1_000_000)
          , act'newPrice' = toSpooky @(Maybe Integer) Nothing
          , act'symbol' = toSpooky TestValues.appSymbol
          }
    val = TestValues.adaValue 100 <> TestValues.oneNft

notForSaleData :: TestData ( 'ForSpending BuiltinData BuiltinData)
notForSaleData = SpendingTest dtm redeemer val
  where
    dtm = toBuiltinData notForSaleDatum

    redeemer =
      toBuiltinData $
        NFT.BuyAct
          { act'bid' = toSpooky @Integer (100 * 1_000_000)
          , act'newPrice' = toSpooky @(Maybe Integer) $ Just 150
          , act'symbol' = toSpooky TestValues.appSymbol
          }
    val = TestValues.adaValue 100 <> TestValues.oneNft

bidNotHighEnoughData :: TestData ( 'ForSpending BuiltinData BuiltinData)
bidNotHighEnoughData = SpendingTest dtm redeemer val
  where
    dtm = toBuiltinData initialAuthorDatum

    redeemer =
      toBuiltinData $
        NFT.BuyAct
          { act'bid' = toSpooky @Integer (90 * 1_000_000)
          , act'newPrice' = toSpooky @(Maybe Integer) Nothing
          , act'symbol' = toSpooky TestValues.appSymbol
          }
    val = TestValues.adaValue 90 <> TestValues.oneNft

ownerNotPaidData :: TestData ( 'ForSpending BuiltinData BuiltinData)
ownerNotPaidData = SpendingTest dtm redeemer val
  where
    dtm = toBuiltinData ownerNotPaidDatum

    redeemer =
      toBuiltinData $
        NFT.BuyAct
          { act'bid' = toSpooky @Integer (100 * 1_000_000)
          , act'newPrice' = toSpooky @(Maybe Integer) Nothing
          , act'symbol' = toSpooky TestValues.appSymbol
          }
    val = TestValues.adaValue 0 <> TestValues.oneNft

inconsistentDatumData :: TestData ( 'ForSpending BuiltinData BuiltinData)
inconsistentDatumData = SpendingTest dtm redeemer val
  where
    dtm = toBuiltinData initialAuthorDatum

    redeemer =
      toBuiltinData $
        NFT.BuyAct
          { act'bid' = toSpooky @Integer (100 * 1_000_000)
          , act'newPrice' = toSpooky @(Maybe Integer) Nothing
          , act'symbol' = toSpooky TestValues.appSymbol
          }
    val = TestValues.adaValue 100 <> TestValues.oneNft

validBuyContext :: ContextBuilder ( 'ForSpending d r)
validBuyContext =
  paysToWallet TestValues.authorWallet (TestValues.adaValue 100)
    <> paysToOther (NFT.txValHash uniqueAsset) oneNft initialAuthorDatum
    <> includeGovHead

bidNotHighEnoughContext :: ContextBuilder ( 'ForSpending d r)
bidNotHighEnoughContext =
  paysToWallet TestValues.authorWallet (TestValues.adaValue 90)
    <> paysToOther (NFT.txValHash uniqueAsset) oneNft initialAuthorDatum
    <> includeGovHead

notForSaleContext :: ContextBuilder ( 'ForSpending d r)
notForSaleContext =
  paysToWallet TestValues.userOneWallet TestValues.oneNft
    <> paysToWallet TestValues.authorWallet (TestValues.adaValue 100)
    <> paysToOther (NFT.txValHash uniqueAsset) oneNft notForSaleDatum
    <> includeGovHead

authorNotPaidContext :: ContextBuilder ( 'ForSpending d r)
authorNotPaidContext =
  paysToWallet TestValues.authorWallet (TestValues.adaValue 5)
    <> paysToOther (NFT.txValHash uniqueAsset) oneNft initialAuthorDatum
    <> includeGovHead

ownerNotPaidContext :: ContextBuilder ( 'ForSpending d r)
ownerNotPaidContext =
  paysToWallet TestValues.authorWallet (TestValues.adaValue 50)
    <> paysToOther (NFT.txValHash uniqueAsset) oneNft ownerNotPaidDatum
    <> includeGovHead

inconsistentDatumContext :: ContextBuilder ( 'ForSpending d r)
inconsistentDatumContext =
  paysToWallet TestValues.userOneWallet TestValues.oneNft
    <> paysToWallet TestValues.authorWallet (TestValues.adaValue 100)
    <> paysToOther (NFT.txValHash uniqueAsset) oneNft inconsistentDatum
    <> includeGovHead

mismathingIdBuyContext :: ContextBuilder ( 'ForSpending d r)
mismathingIdBuyContext =
  paysToWallet TestValues.authorWallet (TestValues.adaValue 100)
    <> paysToOther (NFT.txValHash uniqueAsset) oneNft dtm
    <> includeGovHead
  where
    dtm =
      NFT.NodeDatum $
        initialNode
          { NFT.node'information' = toSpooky ((NFT.node'information initialNode) {NFT.info'id' = toSpooky . NFT.NftId . toSpooky @BuiltinByteString $ "I AM INVALID"})
          }

-- SetPrice test cases

validSetPriceData :: TestData ( 'ForSpending BuiltinData BuiltinData)
validSetPriceData = SpendingTest dtm redeemer val
  where
    dtm = toBuiltinData initialAuthorDatum

    redeemer =
      toBuiltinData $
        NFT.SetPriceAct
          { act'newPrice' = toSpooky @(Maybe Integer) $ Just (150 * 1_000_000)
          , act'symbol' = toSpooky TestValues.appSymbol
          }
    val = TestValues.oneNft

ownerUserOneSetPriceData :: TestData ( 'ForSpending BuiltinData BuiltinData)
ownerUserOneSetPriceData = SpendingTest dtm redeemer val
  where
    dtm = toBuiltinData ownerUserOneDatum

    redeemer =
      toBuiltinData $
        NFT.SetPriceAct
          { act'newPrice' = toSpooky @(Maybe Integer) Nothing
          , act'symbol' = toSpooky TestValues.appSymbol
          }
    val = TestValues.oneNft

validSetPriceContext :: ContextBuilder ( 'ForSpending d r)
validSetPriceContext =
  signedWith (unPaymentPubKeyHash authorPkh)
    -- TODO: choose between `paysToOther NFT.txValHash` and `output` (see below)
    <> paysToOther (NFT.txValHash uniqueAsset) oneNft initialAuthorDatum

-- <> (output $ Output (OwnType $ toBuiltinData initialAuthorDatum) TestValues.oneNft)

ownerUserOneSetPriceContext :: ContextBuilder ( 'ForSpending d r)
ownerUserOneSetPriceContext =
  signedWith (unPaymentPubKeyHash userOnePkh)
    <> paysToOther (NFT.txValHash uniqueAsset) oneNft ownerUserOneDatum

authorNotOwnerSetPriceContext :: ContextBuilder ( 'ForSpending d r)
authorNotOwnerSetPriceContext =
  signedWith (unPaymentPubKeyHash authorPkh)
    <> paysToOther (NFT.txValHash uniqueAsset) oneNft ownerUserOneDatum

mismathingIdSetPriceContext :: ContextBuilder ( 'ForSpending d r)
mismathingIdSetPriceContext =
  signedWith (unPaymentPubKeyHash authorPkh)
    <> paysToOther (NFT.txValHash uniqueAsset) oneNft dtm
  where
    dtm =
      NFT.NodeDatum $
        initialNode
          { NFT.node'information' = toSpooky ((NFT.node'information initialNode) {NFT.info'id' = toSpooky . NFT.NftId . toSpooky @BuiltinByteString $ "I AM INVALID"})
          }

-- todo: fix parametrisation/hard-coding
dealingValidator :: TypedValidator Any
dealingValidator =
  unsafeMkTypedValidator $
    Ledger.mkValidatorScript $
      $$(PlutusTx.compile [||wrap||])
        `PlutusTx.applyCode` ($$(PlutusTx.compile [||NFT.mkTxPolicy||]) `PlutusTx.applyCode` PlutusTx.liftCode uniqueAsset)
  where
    wrap = toTestValidator
