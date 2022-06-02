module Test.EfficientNFT.Script.TokenRestake (test) where

import Prelude ((<>))

import Ledger (
  CurrencySymbol,
  Slot (Slot),
  minAdaTxOut,
  unPaymentPubKeyHash,
 )
import Ledger.Value (
  CurrencySymbol (CurrencySymbol),
  Value,
  assetClassValue,
  singleton,
  unAssetClass,
 )
import Plutus.V1.Ledger.Ada (toValue)
import PlutusTx.Prelude hiding (elem, mempty, (<>))
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.Plutus.Context (
  ContextBuilder,
  Purpose (ForSpending),
  mintsValue,
  paysToPubKey,
  paysToSelf,
  signedWith,
 )
import Test.Tasty.Plutus.Script.Unit (shouldValidate)
import Test.Tasty.Plutus.TestData (TestData (SpendingTest))
import Test.Tasty.Plutus.WithScript (withTestScript)

import Mlabs.EfficientNFT.Token (mkTokenName)
import Mlabs.EfficientNFT.Types
import Test.EfficientNFT.Script.Values (shouldFailWithErr)
import Test.EfficientNFT.Script.Values qualified as TestValues

test :: TestTree
test =
  testGroup
    "Restaking"
    [ localOption TestValues.afterDeadline $
        withTestScript "Restake CNFT after lockupEnd" TestValues.testLockScript $ do
          shouldValidate "Pass with valid data and context" validData validCtx

          shouldFailWithErr
            "Fail when lockup is extended"
            "Current slot smaller than lockup+entered"
            extendedData
            validCtx

          shouldValidate
            "Pass when minting non-sg tokens"
            validData
            mintOtherCtx

          sharedAlwaysInvalid
    , localOption TestValues.afterDeadlineAndLockup $
        withTestScript "Restake CNFT after lockupEnd + lockup" TestValues.testLockScript $ do
          shouldValidate "Pass with valid data and context" validData validCtx

          shouldValidate
            "Pass when lockup is extended"
            extendedData
            extendedCtx

          shouldValidate
            "Pass when minting non-sg tokens"
            validData
            mintOtherCtx

          sharedAlwaysInvalid
    , localOption TestValues.beforeDeadline $
        withTestScript "Restake CNFT before lockupEnd" TestValues.testLockScript $ do
          shouldFailWithErr
            "Fail with valid data and context"
            "Current slot smaller than lockupEnd"
            validData
            validCtx

          sharedAlwaysInvalid
    ]
  where
    sharedAlwaysInvalid = do
      shouldFailWithErr
        "Fail when changing CO value"
        "Values in CO cannot change"
        validData
        spendCOCtx

      shouldFailWithErr
        "Fail when minting Sg"
        "Cannot mint sg"
        validData
        mintSgCtx

      shouldFailWithErr
        "Fail when owner didn't sign"
        "Owner must sign the transaction"
        validData
        noOwnerSignatureCtx

      shouldFailWithErr
        "Fail when no sg in input"
        "Input does not contain sg"
        validData
        noSgCtx

      shouldFailWithErr
        "Fail with invalid pkh data"
        "Owner must sign the transaction"
        invalidPkhData
        validCtx

      shouldFailWithErr
        "Fail with invalid price data"
        "Input does not contain sg"
        invalidPriceData
        validCtx

mockSgCs :: CurrencySymbol
mockSgCs = CurrencySymbol "ff"

collectionNftWithMinAda :: Value
collectionNftWithMinAda = assetClassValue TestValues.collectionNft 1 <> toValue minAdaTxOut

seabugWithMinAda :: Value
seabugWithMinAda = singleton mockSgCs tn 1 <> toValue minAdaTxOut
  where
    tn =
      mkTokenName $
        NftId (snd . unAssetClass $ TestValues.collectionNft) TestValues.nftPrice TestValues.authorPkh

validData :: TestData ( 'ForSpending LockDatum LockAct)
validData = SpendingTest dtm redeemer val
  where
    dtm = LockDatum mockSgCs 1 (snd . unAssetClass $ TestValues.collectionNft)
    redeemer = Restake TestValues.authorPkh TestValues.nftPrice
    val = collectionNftWithMinAda

extendedData :: TestData ( 'ForSpending LockDatum LockAct)
extendedData = SpendingTest dtm redeemer val
  where
    dtm = LockDatum mockSgCs TestValues.testLockupEnd (snd . unAssetClass $ TestValues.collectionNft)
    redeemer = Restake TestValues.authorPkh TestValues.nftPrice
    val = collectionNftWithMinAda

invalidPkhData :: TestData ( 'ForSpending LockDatum LockAct)
invalidPkhData = SpendingTest dtm redeemer val
  where
    dtm = LockDatum mockSgCs 1 (snd . unAssetClass $ TestValues.collectionNft)
    redeemer = Restake TestValues.userOnePkh TestValues.nftPrice
    val = collectionNftWithMinAda

invalidPriceData :: TestData ( 'ForSpending LockDatum LockAct)
invalidPriceData = SpendingTest dtm redeemer val
  where
    dtm = LockDatum mockSgCs 1 (snd . unAssetClass $ TestValues.collectionNft)
    redeemer = Restake TestValues.authorPkh TestValues.newNftPrice
    val = collectionNftWithMinAda

validCtx :: ContextBuilder ( 'ForSpending LockDatum LockAct)
validCtx =
  noOwnerSignatureCtx
    <> signedWith (unPaymentPubKeyHash TestValues.authorPkh)

spendCOCtx :: ContextBuilder ( 'ForSpending LockDatum LockAct)
spendCOCtx =
  paysToSelf
    (toValue minAdaTxOut)
    (LockDatum mockSgCs TestValues.testLockupEnd (snd . unAssetClass $ TestValues.collectionNft))
    <> signedWith (unPaymentPubKeyHash TestValues.authorPkh)
    <> paysToPubKey (unPaymentPubKeyHash TestValues.authorPkh) (seabugWithMinAda <> assetClassValue TestValues.collectionNft 1)

mintSgCtx :: ContextBuilder ( 'ForSpending LockDatum LockAct)
mintSgCtx =
  validCtx
    <> mintsValue (singleton mockSgCs "foo" 1)

noOwnerSignatureCtx :: ContextBuilder ( 'ForSpending LockDatum LockAct)
noOwnerSignatureCtx =
  paysToSelf
    collectionNftWithMinAda
    (LockDatum mockSgCs TestValues.testLockupEnd (snd . unAssetClass $ TestValues.collectionNft))
    <> paysToPubKey (unPaymentPubKeyHash TestValues.authorPkh) seabugWithMinAda

noSgCtx :: ContextBuilder ( 'ForSpending LockDatum LockAct)
noSgCtx =
  paysToSelf
    collectionNftWithMinAda
    (LockDatum mockSgCs TestValues.testLockupEnd (snd . unAssetClass $ TestValues.collectionNft))
    <> signedWith (unPaymentPubKeyHash TestValues.authorPkh)

extendedCtx :: ContextBuilder ( 'ForSpending LockDatum r)
extendedCtx =
  paysToSelf
    collectionNftWithMinAda
    (LockDatum mockSgCs (TestValues.testLockupEnd + Slot TestValues.testLockup) (snd . unAssetClass $ TestValues.collectionNft))
    <> paysToPubKey (unPaymentPubKeyHash TestValues.authorPkh) seabugWithMinAda
    <> signedWith (unPaymentPubKeyHash TestValues.authorPkh)

mintOtherCtx :: ContextBuilder ( 'ForSpending LockDatum LockAct)
mintOtherCtx =
  validCtx
    <> mintsValue (singleton "aabbcc" "Other token" 42)
