module Test.NFT.Script.Minting (
  testMinting,
) where

import Data.Semigroup ((<>))
import Ledger (unPaymentPubKeyHash)
import Ledger qualified
<<<<<<< Updated upstream
import Ledger.Value (AssetClass (..), singleton)
||||||| constructed merge base
import Ledger.Value (AssetClass (..))
=======
import Ledger.Value (AssetClass (..))
import Ledger.Value qualified as Value
>>>>>>> Stashed changes
import PlutusTx qualified
import PlutusTx.Prelude hiding ((<>))
import PlutusTx.Prelude qualified as PlutusPrelude

import Test.NFT.Script.Values as TestValues
import Test.Tasty (TestTree, localOption)
import Test.Tasty.Plutus.TestData (TestData(MintingTest), token)
import Test.Tasty.Plutus.Context
import Test.Tasty.Plutus.Options (TestCurrencySymbol (TestCurrencySymbol), TestTxId (TestTxId))
import Test.Tasty.Plutus.Script.Unit
<<<<<<< Updated upstream
import Test.Tasty.Plutus.TestData
import Test.Tasty.Plutus.WithScript
||||||| constructed merge base
=======
import Test.Tasty.Plutus.Options (TestTxId (TestTxId), TestCurrencySymbol(TestCurrencySymbol))
import Test.Tasty.Plutus.WithScript (withMintingPolicy, toTestMintingPolicy)
>>>>>>> Stashed changes

import Mlabs.NFT.Spooky (toSpooky, toSpookyAssetClass, toSpookyTokenName, unSpookyTokenName)
import Mlabs.NFT.Types qualified as NFT
import Mlabs.NFT.Validation qualified as NFT

testMinting :: TestTree
testMinting = localOption (TestCurrencySymbol TestValues.nftCurrencySymbol) $
  localOption (TestTxId TestValues.testTxId) $
    withMintingPolicy "Test NFT minting policy" nftMintPolicy $ do
      shouldValidate "Valid case" validData validCtx
      shouldn'tValidate "Not minting" validData noMintingCtx
      shouldn'tValidate "No payee" validData noPayeeCtx
      shouldn'tValidate "Pays wrong amount" validData wrongAmountCtx
      shouldn'tValidate "Mismatching id" validData mismatchingIdCtx

<<<<<<< Updated upstream
baseCtx :: ContextBuilder ( 'ForMinting r)
||||||| constructed merge base
baseCtx :: ContextBuilder 'ForMinting
=======
baseCtx :: ContextBuilder ('ForMinting NFT.MintAct)
>>>>>>> Stashed changes
baseCtx =
  -- FIXME: hacky way to pass "UTXO not consumed"
<<<<<<< Updated upstream
  input $ Input (PubKeyType $ unPaymentPubKeyHash TestValues.authorPkh) TestValues.oneAda
||||||| constructed merge base
  input $ Input (PubKeyType TestValues.authorPkh) TestValues.oneAda
=======
  input $ Input (PubKeyType TestValues.authorPkh Nothing) TestValues.oneAda
>>>>>>> Stashed changes

<<<<<<< Updated upstream
mintingCtx :: ContextBuilder ( 'ForMinting r)
mintingCtx = mintsValue $ singleton TestValues.nftCurrencySymbol (unSpookyTokenName TestValues.testTokenName) 1
||||||| constructed merge base
mintingCtx :: ContextBuilder 'ForMinting
mintingCtx = mintsWithSelf TestValues.testTokenName 1
=======
mintingCtx :: ContextBuilder ('ForMinting NFT.MintAct)
mintingCtx = mintsValue (Value.singleton TestValues.nftCurrencySymbol TestValues.testTokenName 1)
>>>>>>> Stashed changes

<<<<<<< Updated upstream
paysNftToScriptCtx :: ContextBuilder ( 'ForMinting r)
paysNftToScriptCtx = paysToOther (NFT.txValHash uniqueAsset) TestValues.oneNft ()
||||||| constructed merge base
paysNftToScriptCtx :: ContextBuilder 'ForMinting
paysNftToScriptCtx = paysOther (NFT.txValHash uniqueAsset) TestValues.oneNft ()
=======
paysNftToScriptCtx :: ContextBuilder ('ForMinting NFT.MintAct)
paysNftToScriptCtx = paysToOther (NFT.txValHash uniqueAsset) TestValues.oneNft ()
>>>>>>> Stashed changes

<<<<<<< Updated upstream
paysDatumToScriptCtx :: ContextBuilder ( 'ForMinting r)
||||||| constructed merge base
paysDatumToScriptCtx :: ContextBuilder 'ForMinting
=======
paysDatumToScriptCtx :: ContextBuilder ('ForMinting NFT.MintAct)
>>>>>>> Stashed changes
paysDatumToScriptCtx =
  spendsFromOther (NFT.txValHash uniqueAsset) TestValues.oneNft (NFT.HeadDatum $ NFT.NftListHead (toSpooky @(Maybe NFT.Pointer) Nothing) (toSpooky TestValues.appInstance))
    <> paysToOther (NFT.txValHash uniqueAsset) mempty nodeDatum
    <> paysToOther (NFT.txValHash uniqueAsset) mempty headDatum
  where
    nodeDatum =
      NFT.NodeDatum $
        NFT.NftListNode
          { node'information' =
              toSpooky $
                NFT.InformationNft
                  { info'id' = toSpooky TestValues.testNftId
                  , info'share' = toSpooky (1 % 2)
                  , info'author' = toSpooky . NFT.UserId . toSpooky $ TestValues.authorPkh
                  , info'owner' = toSpooky . NFT.UserId . toSpooky $ TestValues.authorPkh
                  , info'price' = toSpooky $ Just (100 * 1_000_000 :: Integer)
                  , info'auctionState' = toSpooky @(Maybe NFT.AuctionState) Nothing
                  }
          , node'next' = toSpooky @(Maybe NFT.Pointer) Nothing
          , node'appInstance' = toSpooky TestValues.appInstance
          }
    ptr = NFT.Pointer . toSpooky . toSpookyAssetClass $ AssetClass (TestValues.nftCurrencySymbol, unSpookyTokenName TestValues.testTokenName)
    headDatum = NFT.HeadDatum $ NFT.NftListHead (toSpooky $ Just ptr) (toSpooky TestValues.appInstance)

<<<<<<< Updated upstream
paysWrongAmountCtx :: ContextBuilder ( 'ForMinting r)
||||||| constructed merge base
paysWrongAmountCtx :: ContextBuilder 'ForMinting
=======
paysWrongAmountCtx :: ContextBuilder ('ForMinting NFT.MintAct)
>>>>>>> Stashed changes
paysWrongAmountCtx =
  baseCtx <> mintingCtx
    <> paysToOther
      (NFT.txValHash uniqueAsset)
      (TestValues.oneNft PlutusPrelude.<> TestValues.oneNft)
      TestValues.testNftId

<<<<<<< Updated upstream
validCtx :: ContextBuilder ( 'ForMinting r)
||||||| constructed merge base
validCtx :: ContextBuilder 'ForMinting
=======
validCtx :: ContextBuilder ('ForMinting NFT.MintAct)
>>>>>>> Stashed changes
validCtx = baseCtx <> mintingCtx <> paysNftToScriptCtx <> paysDatumToScriptCtx

<<<<<<< Updated upstream
noMintingCtx :: ContextBuilder ( 'ForMinting r)
||||||| constructed merge base
noMintingCtx :: ContextBuilder 'ForMinting
=======
noMintingCtx :: ContextBuilder ('ForMinting NFT.MintAct)
>>>>>>> Stashed changes
noMintingCtx = baseCtx <> paysNftToScriptCtx <> paysDatumToScriptCtx

<<<<<<< Updated upstream
noPayeeCtx :: ContextBuilder ( 'ForMinting r)
||||||| constructed merge base
noPayeeCtx :: ContextBuilder 'ForMinting
=======
noPayeeCtx :: ContextBuilder ('ForMinting NFT.MintAct)
>>>>>>> Stashed changes
noPayeeCtx = baseCtx <> paysDatumToScriptCtx <> paysNftToScriptCtx

<<<<<<< Updated upstream
validData :: TestData ( 'ForMinting NFT.MintAct)
validData = MintingTest (NFT.Mint $ toSpooky TestValues.testNftId) (token (unSpookyTokenName TestValues.testTokenName) 1)
||||||| constructed merge base
validData :: TestData 'ForMinting
validData = MintingTest (NFT.Mint $ toSpooky TestValues.testNftId)
=======
validData :: TestData ('ForMinting NFT.MintAct)
validData = MintingTest (NFT.Mint $ toSpooky TestValues.testNftId) (token TestValues.testTokenName 1)
>>>>>>> Stashed changes

<<<<<<< Updated upstream
nonMintingCtx :: ContextBuilder ( 'ForMinting r)
||||||| constructed merge base
nonMintingCtx :: ContextBuilder 'ForMinting
=======
nonMintingCtx :: ContextBuilder ('ForMinting NFT.MintAct)
>>>>>>> Stashed changes
nonMintingCtx =
<<<<<<< Updated upstream
  paysToOther (NFT.txValHash uniqueAsset) TestValues.oneNft TestValues.testNftId
    <> input (Input (PubKeyType $ unPaymentPubKeyHash TestValues.authorPkh) TestValues.oneAda)
||||||| constructed merge base
  paysOther (NFT.txValHash uniqueAsset) TestValues.oneNft TestValues.testNftId
    <> input (Input (PubKeyType TestValues.authorPkh) TestValues.oneAda)
=======
  paysToOther (NFT.txValHash uniqueAsset) TestValues.oneNft TestValues.testNftId
    <> input (Input (PubKeyType TestValues.authorPkh Nothing) TestValues.oneAda)
>>>>>>> Stashed changes

<<<<<<< Updated upstream
wrongAmountCtx :: ContextBuilder ( 'ForMinting r)
||||||| constructed merge base
wrongAmountCtx :: ContextBuilder 'ForMinting
=======
wrongAmountCtx :: ContextBuilder ('ForMinting NFT.MintAct)
>>>>>>> Stashed changes
wrongAmountCtx =
  baseCtx <> mintingCtx <> paysDatumToScriptCtx
    <> paysToOther (NFT.txValHash uniqueAsset) (TestValues.oneNft PlutusPrelude.<> TestValues.oneNft) ()

<<<<<<< Updated upstream
mismatchingIdCtx :: ContextBuilder ( 'ForMinting r)
||||||| constructed merge base
mismatchingIdCtx :: ContextBuilder 'ForMinting
=======
mismatchingIdCtx :: ContextBuilder ('ForMinting NFT.MintAct)
>>>>>>> Stashed changes
mismatchingIdCtx =
  baseCtx
    <> mintingCtx
    <> paysNftToScriptCtx
    <> spendsFromOther (NFT.txValHash uniqueAsset) TestValues.oneNft (NFT.HeadDatum $ NFT.NftListHead (toSpooky @(Maybe NFT.Pointer) Nothing) (toSpooky TestValues.appInstance))
    <> paysToOther (NFT.txValHash uniqueAsset) mempty nodeDatum
    <> paysToOther (NFT.txValHash uniqueAsset) mempty headDatum
  where
    nodeDatum =
      NFT.NodeDatum $
        NFT.NftListNode
          { node'information' =
              toSpooky $
                NFT.InformationNft
                  { info'id' = toSpooky . NFT.NftId . toSpooky @BuiltinByteString $ "I AM INVALID"
                  , info'share' = toSpooky (1 % 2)
                  , info'author' = toSpooky . NFT.UserId . toSpooky $ TestValues.authorPkh
                  , info'owner' = toSpooky . NFT.UserId . toSpooky $ TestValues.authorPkh
                  , info'price' = toSpooky $ Just (100 * 1_000_000 :: Integer)
                  , info'auctionState' = toSpooky @(Maybe NFT.AuctionState) Nothing
                  }
          , node'next' = toSpooky @(Maybe NFT.Pointer) Nothing
          , node'appInstance' = toSpooky TestValues.appInstance
          }
    ptr = NFT.Pointer . toSpooky . toSpookyAssetClass $ AssetClass (TestValues.nftCurrencySymbol, unSpookyTokenName TestValues.testTokenName)
    headDatum = NFT.HeadDatum $ NFT.NftListHead (toSpooky $ Just ptr) (toSpooky TestValues.appInstance)

nftMintPolicy :: Ledger.MintingPolicy
nftMintPolicy =
  Ledger.mkMintingPolicyScript $
    $$(PlutusTx.compile [||go||])
      `PlutusTx.applyCode` ( $$(PlutusTx.compile [||NFT.mkMintPolicy||])
                              `PlutusTx.applyCode` PlutusTx.liftCode TestValues.appInstance
                           )
  where
    go = toTestMintingPolicy
