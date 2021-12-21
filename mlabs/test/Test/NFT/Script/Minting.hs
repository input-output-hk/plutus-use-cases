module Test.NFT.Script.Minting (
  testMinting,
) where

import Data.Semigroup ((<>))
import Ledger qualified
import Ledger.Value (AssetClass (..))
import PlutusTx qualified
import PlutusTx.Prelude hiding ((<>))
import PlutusTx.Prelude qualified as PlutusPrelude

import Test.NFT.Script.Values as TestValues
import Test.Tasty (TestTree, localOption)
import Test.Tasty.Plutus.Context
import Test.Tasty.Plutus.Script.Unit

import Mlabs.NFT.Spooky (toSpooky)
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

baseCtx :: ContextBuilder 'ForMinting
baseCtx =
  -- FIXME: hacky way to pass "UTXO not consumed"
  input $ Input (PubKeyType TestValues.authorPkh) TestValues.oneAda

mintingCtx :: ContextBuilder 'ForMinting
mintingCtx = mintsWithSelf TestValues.testTokenName 1

paysNftToScriptCtx :: ContextBuilder 'ForMinting
paysNftToScriptCtx = paysOther (NFT.txValHash uniqueAsset) TestValues.oneNft ()

paysDatumToScriptCtx :: ContextBuilder 'ForMinting
paysDatumToScriptCtx =
  spendsFromOther (NFT.txValHash uniqueAsset) TestValues.oneNft (NFT.HeadDatum $ NFT.NftListHead (toSpooky @(Maybe NFT.Pointer) Nothing) (toSpooky TestValues.appInstance))
    <> paysOther (NFT.txValHash uniqueAsset) mempty nodeDatum
    <> paysOther (NFT.txValHash uniqueAsset) mempty headDatum
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
    ptr = NFT.Pointer . toSpooky $ AssetClass (TestValues.nftCurrencySymbol, TestValues.testTokenName)
    headDatum = NFT.HeadDatum $ NFT.NftListHead (toSpooky $ Just ptr) (toSpooky TestValues.appInstance)

paysWrongAmountCtx :: ContextBuilder 'ForMinting
paysWrongAmountCtx =
  baseCtx <> mintingCtx
    <> paysOther
      (NFT.txValHash uniqueAsset)
      (TestValues.oneNft PlutusPrelude.<> TestValues.oneNft)
      TestValues.testNftId

validCtx :: ContextBuilder 'ForMinting
validCtx = baseCtx <> mintingCtx <> paysNftToScriptCtx <> paysDatumToScriptCtx

noMintingCtx :: ContextBuilder 'ForMinting
noMintingCtx = baseCtx <> paysNftToScriptCtx <> paysDatumToScriptCtx

noPayeeCtx :: ContextBuilder 'ForMinting
noPayeeCtx = baseCtx <> paysDatumToScriptCtx <> paysNftToScriptCtx

validData :: TestData 'ForMinting
validData = MintingTest (NFT.Mint $ toSpooky TestValues.testNftId)

nonMintingCtx :: ContextBuilder 'ForMinting
nonMintingCtx =
  paysOther (NFT.txValHash uniqueAsset) TestValues.oneNft TestValues.testNftId
    <> input (Input (PubKeyType TestValues.authorPkh) TestValues.oneAda)

wrongAmountCtx :: ContextBuilder 'ForMinting
wrongAmountCtx =
  baseCtx <> mintingCtx <> paysDatumToScriptCtx
    <> paysOther (NFT.txValHash uniqueAsset) (TestValues.oneNft PlutusPrelude.<> TestValues.oneNft) ()

mismatchingIdCtx :: ContextBuilder 'ForMinting
mismatchingIdCtx =
  baseCtx
    <> mintingCtx
    <> paysNftToScriptCtx
    <> spendsFromOther (NFT.txValHash uniqueAsset) TestValues.oneNft (NFT.HeadDatum $ NFT.NftListHead (toSpooky @(Maybe NFT.Pointer) Nothing) (toSpooky TestValues.appInstance))
    <> paysOther (NFT.txValHash uniqueAsset) mempty nodeDatum
    <> paysOther (NFT.txValHash uniqueAsset) mempty headDatum
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
    ptr = NFT.Pointer . toSpooky $ AssetClass (TestValues.nftCurrencySymbol, TestValues.testTokenName)
    headDatum = NFT.HeadDatum $ NFT.NftListHead (toSpooky $ Just ptr) (toSpooky TestValues.appInstance)

nftMintPolicy :: Ledger.MintingPolicy
nftMintPolicy =
  Ledger.mkMintingPolicyScript $
    $$(PlutusTx.compile [||go||])
      `PlutusTx.applyCode` ( $$(PlutusTx.compile [||NFT.mkMintPolicy||])
                              `PlutusTx.applyCode` PlutusTx.liftCode TestValues.appInstance
                           )
  where
    go = TestValues.myToTestMintingPolicy
