{-# LANGUAGE QuasiQuotes #-}

module Test.NFT.Script.Minting (
  testMinting,
) where

import Data.Semigroup ((<>))
import Ledger (unPaymentPubKeyHash)
import Ledger.Value (AssetClass (..), singleton)
import PlutusTx qualified
import PlutusTx.Positive (positive)
import PlutusTx.Prelude hiding ((<>))
import PlutusTx.Prelude qualified as PlutusPrelude
import PlutusTx.Ratio qualified as R

import Test.NFT.Script.Values as TestValues
import Test.Tasty (TestTree, localOption)
import Test.Tasty.Plutus.Context
import Test.Tasty.Plutus.Options (TestCurrencySymbol (TestCurrencySymbol), TestTxId (TestTxId))
import Test.Tasty.Plutus.Script.Unit
import Test.Tasty.Plutus.TestData (TestData (MintingTest), Tokens (Tokens), mintTokens)
import Test.Tasty.Plutus.TestScript (TestScript, mkTestMintingPolicy, toTestMintingPolicy)
import Test.Tasty.Plutus.WithScript (withTestScript)

import Mlabs.NFT.Spooky (toSpooky, toSpookyAssetClass, unSpookyTokenName)
import Mlabs.NFT.Types qualified as NFT
import Mlabs.NFT.Validation qualified as NFT

testMinting :: TestTree
testMinting = localOption (TestCurrencySymbol TestValues.nftCurrencySymbol) $
  localOption (TestTxId TestValues.testTxId) $
    withTestScript "Test NFT minting policy" nftMintPolicy $ do
      shouldValidate "Valid case" validData validCtx
      shouldn'tValidate "Not minting" validData noMintingCtx
      shouldn'tValidate "No payee" validData noPayeeCtx
      shouldn'tValidate "Pays wrong amount" validData wrongAmountCtx
      shouldn'tValidate "Mismatching id" validData mismatchingIdCtx

baseCtx :: ContextBuilder ( 'ForMinting NFT.MintAct)
baseCtx =
  -- FIXME: hacky way to pass "UTXO not consumed"
  spendsFromPubKey (unPaymentPubKeyHash TestValues.authorPkh) TestValues.oneAda

mintingCtx :: ContextBuilder ( 'ForMinting NFT.MintAct)
mintingCtx = mintsValue $ singleton TestValues.nftCurrencySymbol (unSpookyTokenName TestValues.testTokenName) 1

paysNftToScriptCtx :: ContextBuilder ( 'ForMinting NFT.MintAct)
paysNftToScriptCtx = paysToOther (NFT.txValHash uniqueAsset) TestValues.oneNft ()

paysDatumToScriptCtx :: ContextBuilder ( 'ForMinting NFT.MintAct)
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
                  , info'share' = toSpooky (R.reduce 1 2)
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

paysWrongAmountCtx :: ContextBuilder ( 'ForMinting NFT.MintAct)
paysWrongAmountCtx =
  baseCtx <> mintingCtx
    <> paysToOther
      (NFT.txValHash uniqueAsset)
      (TestValues.oneNft PlutusPrelude.<> TestValues.oneNft)
      TestValues.testNftId

validCtx :: ContextBuilder ( 'ForMinting NFT.MintAct)
validCtx = baseCtx <> mintingCtx <> paysNftToScriptCtx <> paysDatumToScriptCtx

noMintingCtx :: ContextBuilder ( 'ForMinting NFT.MintAct)
noMintingCtx = baseCtx <> paysNftToScriptCtx <> paysDatumToScriptCtx

noPayeeCtx :: ContextBuilder ( 'ForMinting NFT.MintAct)
noPayeeCtx = baseCtx <> paysDatumToScriptCtx <> paysNftToScriptCtx

validData :: TestData ( 'ForMinting NFT.MintAct)
validData =
  MintingTest
    (NFT.Mint $ toSpooky TestValues.testNftId)
    (mintTokens (Tokens (unSpookyTokenName TestValues.testTokenName) [positive| 1 |]))

nonMintingCtx :: ContextBuilder ( 'ForMinting NFT.MintAct)
nonMintingCtx =
  paysToOther (NFT.txValHash uniqueAsset) TestValues.oneNft TestValues.testNftId
    <> spendsFromPubKey (unPaymentPubKeyHash TestValues.authorPkh) TestValues.oneAda

wrongAmountCtx :: ContextBuilder ( 'ForMinting NFT.MintAct)
wrongAmountCtx =
  baseCtx <> mintingCtx <> paysDatumToScriptCtx
    <> paysToOther (NFT.txValHash uniqueAsset) (TestValues.oneNft PlutusPrelude.<> TestValues.oneNft) ()

mismatchingIdCtx :: ContextBuilder ( 'ForMinting NFT.MintAct)
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
                  , info'share' = toSpooky (R.reduce 1 2)
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

nftMintPolicy :: TestScript ( 'ForMinting NFT.MintAct)
nftMintPolicy =
  mkTestMintingPolicy
    ( $$(PlutusTx.compile [||NFT.mkMintPolicy||])
        `PlutusTx.applyCode` PlutusTx.liftCode TestValues.appInstance
    )
    $$(PlutusTx.compile [||toTestMintingPolicy||])
