module Test.NFT.Script.Dealing (
  testDealing,
) where

import PlutusTx qualified
import PlutusTx.Prelude hiding ((<>))

import Data.Semigroup ((<>))
import Data.Kind (Type)

import Ledger qualified
import Ledger.Typed.Scripts.Validators (TypedValidator,
  ValidatorTypes, RedeemerType, DatumType, mkTypedValidator)
import Test.NFT.Script.Values as TestValues
import Test.Tasty (TestTree)
import Test.Tasty.Plutus.Context (
  ContextBuilder,
  Purpose (ForSpending),
<<<<<<< Updated upstream
  paysToOther,
||||||| constructed merge base
  paysOther,
=======
>>>>>>> Stashed changes
  paysToWallet,
  paysToOther,
  signedWith,
 )
import Test.Tasty.Plutus.WithScript (withValidator, toTestValidator)
import Test.Tasty.Plutus.Script.Unit (
  shouldValidate,
  shouldn'tValidate,
 )
<<<<<<< Updated upstream
import Test.Tasty.Plutus.WithScript
||||||| constructed merge base
=======
import Test.Tasty.Plutus.TestData (TestData (SpendingTest))
>>>>>>> Stashed changes

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

<<<<<<< Updated upstream
validBuyData :: TestData ( 'ForSpending BuiltinData BuiltinData)
||||||| constructed merge base
validBuyData :: TestData 'ForSpending
=======
validBuyData :: TestData ('ForSpending NFT.DatumNft NFT.UserAct)
>>>>>>> Stashed changes
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

<<<<<<< Updated upstream
notForSaleData :: TestData ( 'ForSpending BuiltinData BuiltinData)
||||||| constructed merge base
notForSaleData :: TestData 'ForSpending
=======
notForSaleData :: TestData ('ForSpending NFT.DatumNft NFT.UserAct)
>>>>>>> Stashed changes
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

<<<<<<< Updated upstream
bidNotHighEnoughData :: TestData ( 'ForSpending BuiltinData BuiltinData)
||||||| constructed merge base
bidNotHighEnoughData :: TestData 'ForSpending
=======
bidNotHighEnoughData :: TestData ('ForSpending NFT.DatumNft NFT.UserAct)
>>>>>>> Stashed changes
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

<<<<<<< Updated upstream
ownerNotPaidData :: TestData ( 'ForSpending BuiltinData BuiltinData)
||||||| constructed merge base
ownerNotPaidData :: TestData 'ForSpending
=======
ownerNotPaidData :: TestData ('ForSpending NFT.DatumNft NFT.UserAct)
>>>>>>> Stashed changes
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

<<<<<<< Updated upstream
inconsistentDatumData :: TestData ( 'ForSpending BuiltinData BuiltinData)
||||||| constructed merge base
inconsistentDatumData :: TestData 'ForSpending
=======
inconsistentDatumData :: TestData ('ForSpending NFT.DatumNft NFT.UserAct)
>>>>>>> Stashed changes
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

<<<<<<< Updated upstream
validBuyContext :: ContextBuilder ( 'ForSpending d r)
||||||| constructed merge base
validBuyContext :: ContextBuilder 'ForSpending
=======
validBuyContext :: ContextBuilder ('ForSpending NFT.DatumNft NFT.UserAct)
>>>>>>> Stashed changes
validBuyContext =
  paysToWallet TestValues.authorWallet (TestValues.adaValue 100)
    <> paysToOther (NFT.txValHash uniqueAsset) oneNft initialAuthorDatum
    <> includeGovHead

<<<<<<< Updated upstream
bidNotHighEnoughContext :: ContextBuilder ( 'ForSpending d r)
||||||| constructed merge base
bidNotHighEnoughContext :: ContextBuilder 'ForSpending
=======
bidNotHighEnoughContext :: ContextBuilder ('ForSpending NFT.DatumNft NFT.UserAct)
>>>>>>> Stashed changes
bidNotHighEnoughContext =
  paysToWallet TestValues.authorWallet (TestValues.adaValue 90)
    <> paysToOther (NFT.txValHash uniqueAsset) oneNft initialAuthorDatum
    <> includeGovHead

<<<<<<< Updated upstream
notForSaleContext :: ContextBuilder ( 'ForSpending d r)
||||||| constructed merge base
notForSaleContext :: ContextBuilder 'ForSpending
=======
notForSaleContext :: ContextBuilder ('ForSpending NFT.DatumNft NFT.UserAct)
>>>>>>> Stashed changes
notForSaleContext =
  paysToWallet TestValues.userOneWallet TestValues.oneNft
    <> paysToWallet TestValues.authorWallet (TestValues.adaValue 100)
    <> paysToOther (NFT.txValHash uniqueAsset) oneNft notForSaleDatum
    <> includeGovHead

<<<<<<< Updated upstream
authorNotPaidContext :: ContextBuilder ( 'ForSpending d r)
||||||| constructed merge base
authorNotPaidContext :: ContextBuilder 'ForSpending
=======
authorNotPaidContext :: ContextBuilder ('ForSpending NFT.DatumNft NFT.UserAct)
>>>>>>> Stashed changes
authorNotPaidContext =
  paysToWallet TestValues.authorWallet (TestValues.adaValue 5)
    <> paysToOther (NFT.txValHash uniqueAsset) oneNft initialAuthorDatum
    <> includeGovHead

<<<<<<< Updated upstream
ownerNotPaidContext :: ContextBuilder ( 'ForSpending d r)
||||||| constructed merge base
ownerNotPaidContext :: ContextBuilder 'ForSpending
=======
ownerNotPaidContext :: ContextBuilder ('ForSpending NFT.DatumNft NFT.UserAct)
>>>>>>> Stashed changes
ownerNotPaidContext =
  paysToWallet TestValues.authorWallet (TestValues.adaValue 50)
    <> paysToOther (NFT.txValHash uniqueAsset) oneNft ownerNotPaidDatum
    <> includeGovHead

<<<<<<< Updated upstream
inconsistentDatumContext :: ContextBuilder ( 'ForSpending d r)
||||||| constructed merge base
inconsistentDatumContext :: ContextBuilder 'ForSpending
=======
inconsistentDatumContext :: ContextBuilder ('ForSpending NFT.DatumNft NFT.UserAct)
>>>>>>> Stashed changes
inconsistentDatumContext =
  paysToWallet TestValues.userOneWallet TestValues.oneNft
    <> paysToWallet TestValues.authorWallet (TestValues.adaValue 100)
    <> paysToOther (NFT.txValHash uniqueAsset) oneNft inconsistentDatum
    <> includeGovHead

<<<<<<< Updated upstream
mismathingIdBuyContext :: ContextBuilder ( 'ForSpending d r)
||||||| constructed merge base
mismathingIdBuyContext :: ContextBuilder 'ForSpending
=======
mismathingIdBuyContext :: ContextBuilder ('ForSpending NFT.DatumNft NFT.UserAct)
>>>>>>> Stashed changes
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

<<<<<<< Updated upstream
validSetPriceData :: TestData ( 'ForSpending BuiltinData BuiltinData)
||||||| constructed merge base
validSetPriceData :: TestData 'ForSpending
=======
validSetPriceData :: TestData ('ForSpending NFT.DatumNft NFT.UserAct)
>>>>>>> Stashed changes
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

<<<<<<< Updated upstream
ownerUserOneSetPriceData :: TestData ( 'ForSpending BuiltinData BuiltinData)
||||||| constructed merge base
ownerUserOneSetPriceData :: TestData 'ForSpending
=======
ownerUserOneSetPriceData :: TestData ('ForSpending NFT.DatumNft NFT.UserAct)
>>>>>>> Stashed changes
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

<<<<<<< Updated upstream
validSetPriceContext :: ContextBuilder ( 'ForSpending d r)
||||||| constructed merge base
validSetPriceContext :: ContextBuilder 'ForSpending
=======
validSetPriceContext :: ContextBuilder ('ForSpending NFT.DatumNft NFT.UserAct)
>>>>>>> Stashed changes
validSetPriceContext =
<<<<<<< Updated upstream
  signedWith (unPaymentPubKeyHash authorPkh)
    -- TODO: choose between `paysToOther NFT.txValHash` and `output` (see below)
    <> paysToOther (NFT.txValHash uniqueAsset) oneNft initialAuthorDatum
||||||| constructed merge base
  signedWith authorPkh
    -- TODO: choose between `paysOther NFT.txValHash` and `output` (see below)
    <> paysOther (NFT.txValHash uniqueAsset) oneNft initialAuthorDatum
=======
  signedWith authorPkh
    -- TODO: choose between `paysToOther NFT.txValHash` and `output` (see below)
    <> paysToOther (NFT.txValHash uniqueAsset) oneNft initialAuthorDatum
>>>>>>> Stashed changes

-- <> (output $ Output (OwnType $ toBuiltinData initialAuthorDatum) TestValues.oneNft)

<<<<<<< Updated upstream
ownerUserOneSetPriceContext :: ContextBuilder ( 'ForSpending d r)
||||||| constructed merge base
ownerUserOneSetPriceContext :: ContextBuilder 'ForSpending
=======
ownerUserOneSetPriceContext :: ContextBuilder ('ForSpending NFT.DatumNft NFT.UserAct)
>>>>>>> Stashed changes
ownerUserOneSetPriceContext =
<<<<<<< Updated upstream
  signedWith (unPaymentPubKeyHash userOnePkh)
    <> paysToOther (NFT.txValHash uniqueAsset) oneNft ownerUserOneDatum
||||||| constructed merge base
  signedWith userOnePkh
    <> paysOther (NFT.txValHash uniqueAsset) oneNft ownerUserOneDatum
=======
  signedWith userOnePkh
    <> paysToOther (NFT.txValHash uniqueAsset) oneNft ownerUserOneDatum
>>>>>>> Stashed changes

<<<<<<< Updated upstream
authorNotOwnerSetPriceContext :: ContextBuilder ( 'ForSpending d r)
||||||| constructed merge base
authorNotOwnerSetPriceContext :: ContextBuilder 'ForSpending
=======
authorNotOwnerSetPriceContext :: ContextBuilder ('ForSpending NFT.DatumNft NFT.UserAct)
>>>>>>> Stashed changes
authorNotOwnerSetPriceContext =
<<<<<<< Updated upstream
  signedWith (unPaymentPubKeyHash authorPkh)
    <> paysToOther (NFT.txValHash uniqueAsset) oneNft ownerUserOneDatum
||||||| constructed merge base
  signedWith authorPkh
    <> paysOther (NFT.txValHash uniqueAsset) oneNft ownerUserOneDatum
=======
  signedWith authorPkh
    <> paysToOther (NFT.txValHash uniqueAsset) oneNft ownerUserOneDatum
>>>>>>> Stashed changes

<<<<<<< Updated upstream
mismathingIdSetPriceContext :: ContextBuilder ( 'ForSpending d r)
||||||| constructed merge base
mismathingIdSetPriceContext :: ContextBuilder 'ForSpending
=======
mismathingIdSetPriceContext :: ContextBuilder ('ForSpending NFT.DatumNft NFT.UserAct)
>>>>>>> Stashed changes
mismathingIdSetPriceContext =
<<<<<<< Updated upstream
  signedWith (unPaymentPubKeyHash authorPkh)
    <> paysToOther (NFT.txValHash uniqueAsset) oneNft dtm
||||||| constructed merge base
  signedWith authorPkh
    <> paysOther (NFT.txValHash uniqueAsset) oneNft dtm
=======
  signedWith authorPkh
    <> paysToOther (NFT.txValHash uniqueAsset) oneNft dtm
>>>>>>> Stashed changes
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


-- todo: fix parametrisation/hard-coding
<<<<<<< Updated upstream
dealingValidator :: TypedValidator Any
||||||| constructed merge base
dealingValidator :: Ledger.Validator
=======
dealingValidator :: TypedValidator TestScript
>>>>>>> Stashed changes
dealingValidator =
<<<<<<< Updated upstream
  unsafeMkTypedValidator $
    Ledger.mkValidatorScript $
      $$(PlutusTx.compile [||wrap||])
        `PlutusTx.applyCode` ($$(PlutusTx.compile [||NFT.mkTxPolicy||]) `PlutusTx.applyCode` PlutusTx.liftCode uniqueAsset)
||||||| constructed merge base
  Ledger.mkValidatorScript $
    $$(PlutusTx.compile [||wrap||])
      `PlutusTx.applyCode` ($$(PlutusTx.compile [||NFT.mkTxPolicy||]) `PlutusTx.applyCode` PlutusTx.liftCode uniqueAsset)
=======
  mkTypedValidator @TestScript
    ($$(PlutusTx.compile [||NFT.mkTxPolicy||]) `PlutusTx.applyCode` PlutusTx.liftCode uniqueAsset)
    $$(PlutusTx.compile [||wrap||])
>>>>>>> Stashed changes
  where
<<<<<<< Updated upstream
    wrap = toTestValidator
||||||| constructed merge base
    wrap = TestValues.myToTestValidator
=======
    wrap = myToTestValidator


{-# INLINEABLE myToTestValidator #-}
myToTestValidator ::
  forall (datum :: Type) (redeemer :: Type) (ctx :: Type).
  (PlutusTx.FromData datum, PlutusTx.FromData redeemer, PlutusTx.FromData ctx) =>
  (datum -> redeemer -> ctx -> Bool) ->
  (BuiltinData -> BuiltinData -> BuiltinData -> ())
myToTestValidator f d r p = case fromBuiltinData d of
  Nothing -> reportParseFailed "Datum"
  Just d' -> case fromBuiltinData r of
    Nothing -> reportParseFailed "Redeemer"
    Just r' -> case fromBuiltinData p of
      Nothing -> reportParseFailed "ScriptContext"
      Just p' ->
        if f d' r' p'
          then reportPass
          else reportFail
>>>>>>> Stashed changes
