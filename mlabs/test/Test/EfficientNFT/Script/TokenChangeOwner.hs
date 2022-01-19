module Test.EfficientNFT.Script.TokenChangeOwner (test) where

import Ledger (
  MintingPolicy,
  PaymentPubKeyHash (unPaymentPubKeyHash),
  mkMintingPolicyScript,
  scriptCurrencySymbol,
 )
import Ledger.Ada qualified as Ada
import Ledger.Value (CurrencySymbol, TokenName)
import Ledger.Value qualified as Value
import PlutusTx qualified

import PlutusTx.Prelude hiding (mconcat, (<>))
import Prelude (mconcat, (<>))

import Test.Tasty (TestTree, localOption)
import Test.Tasty.Plutus.Context (
  ContextBuilder,
  Purpose (ForMinting),
  paysToPubKey,
  paysToPubKeyWithDatum,
  spendsFromPubKey,
 )
import Test.Tasty.Plutus.Options (TestCurrencySymbol (TestCurrencySymbol))
import Test.Tasty.Plutus.Script.Unit (shouldValidate)
import Test.Tasty.Plutus.TestData (TestData (MintingTest), token)
import Test.Tasty.Plutus.WithScript (toTestMintingPolicy, withMintingPolicy)

import Mlabs.EfficientNFT.Token (mkPolicy, mkTokenName)
import Mlabs.EfficientNFT.Types (
  MintAct (ChangeOwner),
  OwnerData (OwnerData),
 )

import Test.EfficientNFT.Script.Values qualified as TestValues

test :: TestTree
test =
  localOption (TestCurrencySymbol testTokenCurSym) $
    withMintingPolicy "Token change owner" testTokenPolicy $ do
      shouldValidate "valid buy" validData validCtx

validData :: TestData ( 'ForMinting MintAct)
validData = MintingTest redeemer tokens
  where
    tokens = token validOldTokenName (-1) <> token validNewTokenName 1
    redeemer = ChangeOwner (OwnerData TestValues.userOnePkh TestValues.nftPrice) TestValues.userTwoPkh

validOldTokenName :: TokenName
validOldTokenName = mkTokenName TestValues.userOnePkh TestValues.nftPrice

validNewTokenName :: TokenName
validNewTokenName = mkTokenName TestValues.userTwoPkh TestValues.nftPrice

validCtx :: ContextBuilder ( 'ForMinting MintAct)
validCtx =
  mconcat
    [ spendsFromPubKey
        (unPaymentPubKeyHash TestValues.userOnePkh)
        (Value.singleton testTokenCurSym validOldTokenName 1)
    , spendsFromPubKey
        (unPaymentPubKeyHash TestValues.userTwoPkh)
        (Ada.lovelaceValueOf (fromEnum TestValues.nftPrice))
    , paysToPubKeyWithDatum
        (unPaymentPubKeyHash TestValues.authorPkh)
        TestValues.authorShareVal
        testTokenCurSym
    , paysToPubKeyWithDatum
        (unPaymentPubKeyHash TestValues.platformPkh)
        TestValues.marketplShareVal
        testTokenCurSym
    , paysToPubKeyWithDatum
        (unPaymentPubKeyHash TestValues.userOnePkh)
        TestValues.ownerShareVal
        testTokenCurSym
    , paysToPubKey
        (unPaymentPubKeyHash TestValues.userTwoPkh)
        (Value.singleton testTokenCurSym validNewTokenName 1)
    ]

testTokenCurSym :: CurrencySymbol
testTokenCurSym = scriptCurrencySymbol testTokenPolicy

testTokenPolicy :: MintingPolicy
testTokenPolicy =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||go||])
      `PlutusTx.applyCode` ( $$(PlutusTx.compile [||mkPolicy||])
                              `PlutusTx.applyCode` PlutusTx.liftCode TestValues.mintTxOutRef
                              `PlutusTx.applyCode` PlutusTx.liftCode TestValues.authorPkh
                              `PlutusTx.applyCode` PlutusTx.liftCode TestValues.authorShare
                              `PlutusTx.applyCode` PlutusTx.liftCode TestValues.platformCfg
                              `PlutusTx.applyCode` PlutusTx.liftCode TestValues.contentHash
                           )
  where
    go = toTestMintingPolicy
