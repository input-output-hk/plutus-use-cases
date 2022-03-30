module Test.EfficientNFT.Script.FeeWithdraw (test) where

import Prelude

import Ledger (
  CurrencySymbol,
  minAdaTxOut,
  unPaymentPubKeyHash,
 )
import Ledger.Value (CurrencySymbol (CurrencySymbol), Value, assetClassValue, singleton, unAssetClass)
import Plutus.V1.Ledger.Ada (lovelaceValueOf, toValue)
import Plutus.V1.Ledger.Api (toBuiltinData)
import PlutusTx.Prelude (BuiltinData)
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.Plutus.Context (ContextBuilder, Purpose (ForSpending), mintsValue, paysToSelf, paysToWallet, signedWith)
import Test.Tasty.Plutus.Script.Unit (shouldValidate, shouldn'tValidate)
import Test.Tasty.Plutus.TestData (TestData (SpendingTest))
import Test.Tasty.Plutus.WithScript (withTestScript)

import Mlabs.EfficientNFT.Types
import Test.EfficientNFT.Script.Values (shouldFailWithErr)
import Test.EfficientNFT.Script.Values qualified as TestValues

test :: TestTree
test = withTestScript "Fee withdraw" TestValues.testDaoScript $ do
  shouldValidate "Valid withdraw - pkh1" validData validCtx1
  shouldValidate "Valid withdraw - pkh2" validData validCtx2
  shouldValidate "Valid withdraw - many signatures" validData manyPkhsCtx
  shouldn'tValidate "Fail with wrong pkh" validData invalidPkhCtx
  shouldn'tValidate "Fail with no signature" validData noPkhCtx

dtm :: BuiltinData
dtm = toBuiltinData ()

redeemer :: BuiltinData
redeemer = toBuiltinData ()

validData :: TestData ( 'ForSpending BuiltinData BuiltinData)
validData = SpendingTest dtm redeemer val
  where
    val = lovelaceValueOf 5_000_000

validCtx1 :: ContextBuilder ( 'ForSpending BuiltinData BuiltinData)
validCtx1 = signedWith (unPaymentPubKeyHash TestValues.userOnePkh)

validCtx2 :: ContextBuilder ( 'ForSpending BuiltinData BuiltinData)
validCtx2 = signedWith (unPaymentPubKeyHash TestValues.userTwoPkh)

manyPkhsCtx :: ContextBuilder ( 'ForSpending BuiltinData BuiltinData)
manyPkhsCtx =
  mconcat
    [ signedWith (unPaymentPubKeyHash TestValues.userOnePkh)
    , signedWith (unPaymentPubKeyHash TestValues.userTwoPkh)
    , signedWith (unPaymentPubKeyHash TestValues.otherPkh)
    ]

invalidPkhCtx :: ContextBuilder ( 'ForSpending BuiltinData BuiltinData)
invalidPkhCtx = signedWith (unPaymentPubKeyHash TestValues.otherPkh)

noPkhCtx :: ContextBuilder ( 'ForSpending BuiltinData BuiltinData)
noPkhCtx = mempty
