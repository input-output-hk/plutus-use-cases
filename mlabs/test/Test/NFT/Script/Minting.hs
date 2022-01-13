module Test.NFT.Script.Minting (
  testMinting,
) where

import Data.Semigroup ((<>))
import Ledger (unPaymentPubKeyHash)
import Ledger qualified
import Ledger.Value (AssetClass (..), singleton)
import PlutusTx qualified
import PlutusTx.Prelude hiding ((<>))
import PlutusTx.Prelude qualified as PlutusPrelude

import Test.NFT.Script.Values as TestValues
import Test.Tasty (TestTree, localOption)
import Test.Tasty.Plutus.Context
import Test.Tasty.Plutus.Options (TestCurrencySymbol (TestCurrencySymbol), TestTxId (TestTxId))
import Test.Tasty.Plutus.Script.Unit
import Test.Tasty.Plutus.TestData
import Test.Tasty.Plutus.WithScript

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

baseCtx :: ContextBuilder ( 'ForMinting r)
baseCtx =
  -- FIXME: hacky way to pass "UTXO not consumed"
  input $ Input (PubKeyType $ unPaymentPubKeyHash TestValues.authorPkh) TestValues.oneAda

mintingCtx :: ContextBuilder ( 'ForMinting r)
mintingCtx = mintsValue $ singleton TestValues.nftCurrencySymbol (unSpookyTokenName TestValues.testTokenName) 1

paysNftToScriptCtx :: ContextBuilder ( 'ForMinting r)
paysNftToScriptCtx = paysToOther (NFT.txValHash uniqueAsset) TestValues.oneNft ()

paysDatumToScriptCtx :: ContextBuilder ( 'ForMinting r)
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

paysWrongAmountCtx :: ContextBuilder ( 'ForMinting r)
paysWrongAmountCtx =
  baseCtx <> mintingCtx
    <> paysToOther
      (NFT.txValHash uniqueAsset)
      (TestValues.oneNft PlutusPrelude.<> TestValues.oneNft)
      TestValues.testNftId

validCtx :: ContextBuilder ( 'ForMinting r)
validCtx = baseCtx <> mintingCtx <> paysNftToScriptCtx <> paysDatumToScriptCtx

noMintingCtx :: ContextBuilder ( 'ForMinting r)
noMintingCtx = baseCtx <> paysNftToScriptCtx <> paysDatumToScriptCtx

noPayeeCtx :: ContextBuilder ( 'ForMinting r)
noPayeeCtx = baseCtx <> paysDatumToScriptCtx <> paysNftToScriptCtx

validData :: TestData ( 'ForMinting NFT.MintAct)
validData = MintingTest (NFT.Mint $ toSpooky TestValues.testNftId) (token (unSpookyTokenName TestValues.testTokenName) 1)

nonMintingCtx :: ContextBuilder ( 'ForMinting r)
nonMintingCtx =
  paysToOther (NFT.txValHash uniqueAsset) TestValues.oneNft TestValues.testNftId
    <> input (Input (PubKeyType $ unPaymentPubKeyHash TestValues.authorPkh) TestValues.oneAda)

wrongAmountCtx :: ContextBuilder ( 'ForMinting r)
wrongAmountCtx =
  baseCtx <> mintingCtx <> paysDatumToScriptCtx
    <> paysToOther (NFT.txValHash uniqueAsset) (TestValues.oneNft PlutusPrelude.<> TestValues.oneNft) ()

mismatchingIdCtx :: ContextBuilder ( 'ForMinting r)
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
