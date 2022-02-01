module Test.EfficientNFT.Script.TokenUnstake (test) where

import Prelude ((<>))

import Ledger (
  CurrencySymbol,
  minAdaTxOut,
 )
import Ledger.Value (CurrencySymbol (CurrencySymbol), Value, assetClassValue, singleton, unAssetClass)
import Plutus.V1.Ledger.Ada (toValue)
import PlutusTx.Prelude hiding (elem, mempty, (<>))
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.Plutus.Context (ContextBuilder, Purpose (ForSpending), mintsValue, paysToSelf, paysToWallet)
import Test.Tasty.Plutus.Script.Unit (shouldValidate)
import Test.Tasty.Plutus.TestData (TestData (SpendingTest))
import Test.Tasty.Plutus.WithScript (withTestScript)

import Mlabs.EfficientNFT.Types
import Test.EfficientNFT.Script.Values (shouldFailWithErr)
import Test.EfficientNFT.Script.Values qualified as TestValues

test :: TestTree
test =
  testGroup
    "Unstaking"
    [ localOption TestValues.afterDeadline $
        withTestScript "Unstake CNFT after lockupEnd" TestValues.testLockScript $ do
          shouldValidate "Pass with valid data and context" validData validCtx

          shouldFailWithErr
            "Fail when lockup is extended"
            "Current slot smaller than lockup+entered"
            extendedData
            validCtx

          sharedAlwaysInvalid
    , localOption TestValues.afterDeadlineAndLockup $
        withTestScript "Unstake CNFT after lockupEnd + lockup" TestValues.testLockScript $ do
          shouldValidate "Pass with valid data and context" validData validCtx

          shouldValidate
            "Pass when lockup is extended"
            extendedData
            validCtx

          sharedAlwaysInvalid
    , localOption TestValues.beforeDeadline $
        withTestScript "Unstake CNFT before lockupEnd" TestValues.testLockScript $ do
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
        "Fail when not burning sg"
        "sgNFT must be burned"
        validData
        noBurnCtx

      shouldFailWithErr
        "Fail when CO exsits"
        "CO must not exists"
        validData
        withCoCtx

      shouldFailWithErr
        "Fail with invalid pkh data"
        "sgNFT must be burned"
        invalidPkhData
        validCtx

      shouldFailWithErr
        "Fail with invalid price data"
        "sgNFT must be burned"
        invalidPriceData
        validCtx

      shouldFailWithErr
        "Fail with invalid Sg CS"
        "sgNFT must be burned"
        validData
        invalidSgCsCtx

      shouldFailWithErr
        "Fail with invalid Sg TN"
        "sgNFT must be burned"
        validData
        invalidSgTnCtx

mockSgCs :: CurrencySymbol
mockSgCs = CurrencySymbol "ff"

collectionNftWithMinAda :: Value
collectionNftWithMinAda = assetClassValue TestValues.collectionNft 1 <> toValue minAdaTxOut

validData :: TestData ( 'ForSpending LockDatum LockAct)
validData = SpendingTest dtm redeemer val
  where
    dtm = LockDatum mockSgCs 1 (snd . unAssetClass $ TestValues.collectionNft)
    redeemer = Unstake TestValues.authorPkh TestValues.nftPrice
    val = collectionNftWithMinAda

extendedData :: TestData ( 'ForSpending LockDatum LockAct)
extendedData = SpendingTest dtm redeemer val
  where
    dtm = LockDatum mockSgCs TestValues.testLockupEnd (snd . unAssetClass $ TestValues.collectionNft)
    redeemer = Unstake TestValues.authorPkh TestValues.nftPrice
    val = collectionNftWithMinAda

invalidPkhData :: TestData ( 'ForSpending LockDatum LockAct)
invalidPkhData = SpendingTest dtm redeemer val
  where
    dtm = LockDatum mockSgCs 1 (snd . unAssetClass $ TestValues.collectionNft)
    redeemer = Unstake TestValues.userOnePkh TestValues.nftPrice
    val = collectionNftWithMinAda

invalidPriceData :: TestData ( 'ForSpending LockDatum LockAct)
invalidPriceData = SpendingTest dtm redeemer val
  where
    dtm = LockDatum mockSgCs 1 (snd . unAssetClass $ TestValues.collectionNft)
    redeemer = Unstake TestValues.authorPkh TestValues.newNftPrice
    val = collectionNftWithMinAda

validCtx :: ContextBuilder ( 'ForSpending LockDatum LockAct)
validCtx =
  noBurnCtx
    <> mintsValue (singleton mockSgCs TestValues.tokenName (negate 1))

noBurnCtx :: ContextBuilder ( 'ForSpending LockDatum LockAct)
noBurnCtx =
  paysToWallet
    TestValues.authorWallet
    collectionNftWithMinAda

withCoCtx :: ContextBuilder ( 'ForSpending LockDatum LockAct)
withCoCtx =
  validCtx
    <> paysToSelf
      (toValue minAdaTxOut)
      (LockDatum mockSgCs 1 (snd . unAssetClass $ TestValues.collectionNft))

invalidSgCsCtx :: ContextBuilder ( 'ForSpending LockDatum LockAct)
invalidSgCsCtx =
  noBurnCtx
    <> mintsValue (singleton "aa" TestValues.tokenName (negate 1))

invalidSgTnCtx :: ContextBuilder ( 'ForSpending LockDatum LockAct)
invalidSgTnCtx =
  noBurnCtx
    <> mintsValue (singleton mockSgCs "I AM INVALID" (negate 1))
