{-# LANGUAGE QuasiQuotes #-}

module Test.EfficientNFT.Script.TokenBurn (test) where

import Prelude qualified as Hask

import Data.Data (Typeable)
import Data.List.NonEmpty (NonEmpty)
import Data.String (String)
import Ledger (PaymentPubKeyHash (unPaymentPubKeyHash), minAdaTxOut)
import Ledger.Value (CurrencySymbol, TokenName (TokenName, unTokenName))
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Ada (toValue)
import PlutusTx.Positive (Positive, positive)
import PlutusTx.Prelude hiding (elem, (<>))
import Test.Tasty (TestTree, localOption)
import Test.Tasty.Plutus.Context (
  ContextBuilder,
  Purpose (ForMinting),
  makeIncompleteContexts,
  mintsValue,
  paysToOther,
  paysToPubKey,
  paysToPubKeyWithDatum,
  signedWith,
  spendsFromOther,
  spendsFromPubKey,
 )
import Test.Tasty.Plutus.Options (TestCurrencySymbol (TestCurrencySymbol))
import Test.Tasty.Plutus.Script.Property (scriptPropertyFail)
import Test.Tasty.Plutus.Script.Unit (shouldValidate, shouldn'tValidate, shouldn'tValidateTracing)
import Test.Tasty.Plutus.TestData (
  Generator (GenForMinting),
  MintingPolicyTask,
  Outcome,
  TestData (MintingTest),
  TestItems (ItemsForMinting, mpCB, mpOutcome, mpRedeemer, mpTasks),
  Tokens (Tokens),
  burnTokens,
  fromArbitrary,
  mintTokens,
  passIf,
 )
import Test.Tasty.Plutus.WithScript (WithScript, withTestScript)
import Prelude (elem, (<>))

import Mlabs.EfficientNFT.Token (mkTokenName)
import Mlabs.EfficientNFT.Types (MintAct (BurnToken, ChangeOwner))
import Test.EfficientNFT.Script.Values (shouldFailWithErr)
import Test.EfficientNFT.Script.Values qualified as TestValues

test :: TestTree
test =
  withTestScript "Burn token" TestValues.testTokenPolicy $ do
    shouldValidate "Valid burn" validData validCtx

    shouldFailWithErr
      "Fail when minting other Sg"
      "NFT must be burned"
      additionalMintData
      validCtx

    shouldFailWithErr
      "Fail when no owner signature"
      "Owner must sign the transaction"
      validData
      noSignCtx

    shouldFailWithErr
      "Fail when not burning Sg"
      "NFT must be burned"
      noBurnData
      validCtx

    shouldFailWithErr
      "Fail when not unlocking CNFT"
      "Underlying NFT must be unlocked"
      validData
      noUnlockCtx

validData :: TestData ( 'ForMinting MintAct)
validData =
  MintingTest
    redeemer
    (burnTokens (Tokens TestValues.tokenName [positive| 1 |]))
  where
    redeemer = BurnToken TestValues.nft1

additionalMintData =
  MintingTest
    redeemer
    (burnTokens (Tokens TestValues.tokenName [positive| 1 |]) <> mintTokens (Tokens "foo" [positive| 1 |]))
  where
    redeemer = BurnToken TestValues.nft1

noBurnData =
  MintingTest
    redeemer
    (mintTokens (Tokens "foo" [positive| 1 |]))
  where
    redeemer = BurnToken TestValues.nft1

validCtx :: ContextBuilder ( 'ForMinting MintAct)
validCtx =
  noSignCtx
    <> signedWith (unPaymentPubKeyHash TestValues.authorPkh)

noSignCtx :: ContextBuilder ( 'ForMinting MintAct)
noSignCtx =
  spendsFromOther TestValues.burnHash cnft ()
    <> paysToPubKey (unPaymentPubKeyHash TestValues.authorPkh) cnft
  where
    cnft = Value.assetClassValue TestValues.collectionNft 1

noUnlockCtx :: ContextBuilder ( 'ForMinting MintAct)
noUnlockCtx = signedWith (unPaymentPubKeyHash TestValues.authorPkh)
