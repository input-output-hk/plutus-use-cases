module Test.NFT.Script.Dealing (
  testDealing,
) where

import PlutusTx qualified
import PlutusTx.Prelude hiding ((<>))

import Data.Semigroup ((<>))

import Ledger (ScriptContext, unPaymentPubKeyHash)
import Ledger.Typed.Scripts.Validators (
  DatumType,
  RedeemerType,
  TypedValidator,
  ValidatorTypes,
  mkTypedValidator,
 )
import Test.NFT.Script.Values qualified as TestValues
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
import Test.Tasty.Plutus.TestData (TestData (SpendingTest))
import Test.Tasty.Plutus.WithScript (toTestValidator, withValidator)

import Mlabs.NFT.Spooky (toSpooky)
import Mlabs.NFT.Spooky qualified as Spooky
import Mlabs.NFT.Types qualified as NFT
import Mlabs.NFT.Validation qualified as NFT

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

validBuyData :: TestData ( 'ForSpending NFT.DatumNft NFT.UserAct)
validBuyData = SpendingTest dtm redeemer val
  where
    dtm = initialAuthorDatum

    redeemer =
      NFT.BuyAct
        { act'bid' = toSpooky @Integer (100 * 1_000_000)
        , act'newPrice' = toSpooky @(Maybe Integer) Nothing
        , act'symbol' = toSpooky TestValues.appSymbol
        }
    val = TestValues.adaValue 100 <> TestValues.oneNft

notForSaleData :: TestData ( 'ForSpending NFT.DatumNft NFT.UserAct)
notForSaleData = SpendingTest dtm redeemer val
  where
    dtm = notForSaleDatum

    redeemer =
      NFT.BuyAct
        { act'bid' = toSpooky @Integer (100 * 1_000_000)
        , act'newPrice' = toSpooky @(Maybe Integer) $ Just 150
        , act'symbol' = toSpooky TestValues.appSymbol
        }
    val = TestValues.adaValue 100 <> TestValues.oneNft

bidNotHighEnoughData :: TestData ( 'ForSpending NFT.DatumNft NFT.UserAct)
bidNotHighEnoughData = SpendingTest dtm redeemer val
  where
    dtm = initialAuthorDatum

    redeemer =
      NFT.BuyAct
        { act'bid' = toSpooky @Integer (90 * 1_000_000)
        , act'newPrice' = toSpooky @(Maybe Integer) Nothing
        , act'symbol' = toSpooky TestValues.appSymbol
        }
    val = TestValues.adaValue 90 <> TestValues.oneNft

ownerNotPaidData :: TestData ( 'ForSpending NFT.DatumNft NFT.UserAct)
ownerNotPaidData = SpendingTest dtm redeemer val
  where
    dtm = ownerNotPaidDatum

    redeemer =
      NFT.BuyAct
        { act'bid' = toSpooky @Integer (100 * 1_000_000)
        , act'newPrice' = toSpooky @(Maybe Integer) Nothing
        , act'symbol' = toSpooky TestValues.appSymbol
        }
    val = TestValues.adaValue 0 <> TestValues.oneNft

inconsistentDatumData :: TestData ( 'ForSpending NFT.DatumNft NFT.UserAct)
inconsistentDatumData = SpendingTest dtm redeemer val
  where
    dtm = initialAuthorDatum

    redeemer =
      NFT.BuyAct
        { act'bid' = toSpooky @Integer (100 * 1_000_000)
        , act'newPrice' = toSpooky @(Maybe Integer) Nothing
        , act'symbol' = toSpooky TestValues.appSymbol
        }
    val = TestValues.adaValue 100 <> TestValues.oneNft

validBuyContext :: ContextBuilder ( 'ForSpending NFT.DatumNft NFT.UserAct)
validBuyContext =
  paysToWallet TestValues.authorWallet (TestValues.adaValue 100)
    <> paysToOther (NFT.txValHash TestValues.uniqueAsset) TestValues.oneNft initialAuthorDatum
    <> TestValues.includeGovHead

bidNotHighEnoughContext :: ContextBuilder ( 'ForSpending NFT.DatumNft NFT.UserAct)
bidNotHighEnoughContext =
  paysToWallet TestValues.authorWallet (TestValues.adaValue 90)
    <> paysToOther (NFT.txValHash TestValues.uniqueAsset) TestValues.oneNft initialAuthorDatum
    <> TestValues.includeGovHead

notForSaleContext :: ContextBuilder ( 'ForSpending NFT.DatumNft NFT.UserAct)
notForSaleContext =
  paysToWallet TestValues.userOneWallet TestValues.oneNft
    <> paysToWallet TestValues.authorWallet (TestValues.adaValue 100)
    <> paysToOther (NFT.txValHash TestValues.uniqueAsset) TestValues.oneNft notForSaleDatum
    <> TestValues.includeGovHead

authorNotPaidContext :: ContextBuilder ( 'ForSpending NFT.DatumNft NFT.UserAct)
authorNotPaidContext =
  paysToWallet TestValues.authorWallet (TestValues.adaValue 5)
    <> paysToOther (NFT.txValHash TestValues.uniqueAsset) TestValues.oneNft initialAuthorDatum
    <> TestValues.includeGovHead

ownerNotPaidContext :: ContextBuilder ( 'ForSpending NFT.DatumNft NFT.UserAct)
ownerNotPaidContext =
  paysToWallet TestValues.authorWallet (TestValues.adaValue 50)
    <> paysToOther (NFT.txValHash TestValues.uniqueAsset) TestValues.oneNft ownerNotPaidDatum
    <> TestValues.includeGovHead

inconsistentDatumContext :: ContextBuilder ( 'ForSpending NFT.DatumNft NFT.UserAct)
inconsistentDatumContext =
  paysToWallet TestValues.userOneWallet TestValues.oneNft
    <> paysToWallet TestValues.authorWallet (TestValues.adaValue 100)
    <> paysToOther (NFT.txValHash TestValues.uniqueAsset) TestValues.oneNft inconsistentDatum
    <> TestValues.includeGovHead

mismathingIdBuyContext :: ContextBuilder ( 'ForSpending NFT.DatumNft NFT.UserAct)
mismathingIdBuyContext =
  paysToWallet TestValues.authorWallet (TestValues.adaValue 100)
    <> paysToOther (NFT.txValHash TestValues.uniqueAsset) TestValues.oneNft dtm
    <> TestValues.includeGovHead
  where
    dtm =
      NFT.NodeDatum $
        initialNode
          { NFT.node'information' = toSpooky ((NFT.node'information initialNode) {NFT.info'id' = toSpooky . NFT.NftId . toSpooky @BuiltinByteString $ "I AM INVALID"})
          }

-- SetPrice test cases

validSetPriceData :: TestData ( 'ForSpending NFT.DatumNft NFT.UserAct)
validSetPriceData = SpendingTest dtm redeemer val
  where
    dtm = initialAuthorDatum

    redeemer =
      NFT.SetPriceAct
        { act'newPrice' = toSpooky @(Maybe Integer) $ Just (150 * 1_000_000)
        , act'symbol' = toSpooky TestValues.appSymbol
        }
    val = TestValues.oneNft

ownerUserOneSetPriceData :: TestData ( 'ForSpending NFT.DatumNft NFT.UserAct)
ownerUserOneSetPriceData = SpendingTest dtm redeemer val
  where
    dtm = ownerUserOneDatum

    redeemer =
      NFT.SetPriceAct
        { act'newPrice' = toSpooky @(Maybe Integer) Nothing
        , act'symbol' = toSpooky TestValues.appSymbol
        }
    val = TestValues.oneNft

validSetPriceContext :: ContextBuilder ( 'ForSpending NFT.DatumNft NFT.UserAct)
validSetPriceContext =
  signedWith (unPaymentPubKeyHash TestValues.authorPkh)
    -- TODO: choose between `paysToOther NFT.txValHash` and `output` (see below)
    <> paysToOther (NFT.txValHash TestValues.uniqueAsset) TestValues.oneNft initialAuthorDatum

-- <> (output $ Output (OwnType $ toBuiltinData initialAuthorDatum) TestValues.oneNft)

ownerUserOneSetPriceContext :: ContextBuilder ( 'ForSpending NFT.DatumNft NFT.UserAct)
ownerUserOneSetPriceContext =
  signedWith (unPaymentPubKeyHash TestValues.userOnePkh)
    <> paysToOther (NFT.txValHash TestValues.uniqueAsset) TestValues.oneNft ownerUserOneDatum

authorNotOwnerSetPriceContext :: ContextBuilder ( 'ForSpending NFT.DatumNft NFT.UserAct)
authorNotOwnerSetPriceContext =
  signedWith (unPaymentPubKeyHash TestValues.authorPkh)
    <> paysToOther (NFT.txValHash TestValues.uniqueAsset) TestValues.oneNft ownerUserOneDatum

mismathingIdSetPriceContext :: ContextBuilder ( 'ForSpending NFT.DatumNft NFT.UserAct)
mismathingIdSetPriceContext =
  signedWith (unPaymentPubKeyHash TestValues.authorPkh)
    <> paysToOther (NFT.txValHash TestValues.uniqueAsset) TestValues.oneNft dtm
  where
    dtm =
      NFT.NodeDatum $
        initialNode
          { NFT.node'information' = toSpooky ((NFT.node'information initialNode) {NFT.info'id' = toSpooky . NFT.NftId . toSpooky @BuiltinByteString $ "I AM INVALID"})
          }

data TestScript

instance ValidatorTypes TestScript where
  type RedeemerType TestScript = NFT.UserAct
  type DatumType TestScript = NFT.DatumNft

dealingValidator :: TypedValidator TestScript
dealingValidator =
  Spooky.mkTypedValidator @TestScript
    ($$(PlutusTx.compile [||NFT.mkTxPolicy||]) `PlutusTx.applyCode` PlutusTx.liftCode TestValues.uniqueAsset)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap ::
      (NFT.DatumNft -> NFT.UserAct -> Spooky.ScriptContext -> Bool) ->
      (BuiltinData -> BuiltinData -> BuiltinData -> ())
    wrap = toTestValidator
