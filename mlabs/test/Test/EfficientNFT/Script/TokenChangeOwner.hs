module Test.EfficientNFT.Script.TokenChangeOwner (test) where

import Ledger (
  MintingPolicy,
  PaymentPubKeyHash (unPaymentPubKeyHash),
  mkMintingPolicyScript,
  scriptCurrencySymbol,
 )
import Ledger.Value (CurrencySymbol, TokenName (TokenName, unTokenName))
import Ledger.Value qualified as Value
import PlutusTx qualified

import Data.Data (Typeable)
import Data.String (String)
import PlutusTx.Prelude hiding (elem, (<>))
import Prelude (elem, (<>))

import Test.Tasty (TestTree, localOption)
import Test.Tasty.Plutus.Context (
  ContextBuilder,
  Purpose (ForMinting),
  makeIncompleteContexts,
  mintsValue,
  paysToOther,
  paysToPubKeyWithDatum,
 )
import Test.Tasty.Plutus.Options (TestCurrencySymbol (TestCurrencySymbol))
import Test.Tasty.Plutus.Script.Unit (shouldValidate, shouldn'tValidateTracing)
import Test.Tasty.Plutus.TestData (TestData (MintingTest), token)
import Test.Tasty.Plutus.WithScript (WithScript, toTestMintingPolicy, withMintingPolicy)

import Mlabs.EfficientNFT.Token (mkPolicy, mkTokenName)
import Mlabs.EfficientNFT.Types (MintAct (ChangeOwner))

import Test.EfficientNFT.Script.Values qualified as TestValues

test :: TestTree
test =
  localOption (TestCurrencySymbol testTokenCurSym) $
    withMintingPolicy "Token change owner" testTokenPolicy $ do
      shouldValidate "valid buy" validData validCtx
      shouldFailWithErr
        "Fail if token has wrong name"
        "Old version must be burnt when reminting"
        badTokenNameData
        validCtx

      shouldFailWithErr
        "Fail if old token is not burnt"
        "Old version must be burnt when reminting"
        oldTokenNotBurntData
        validCtx

      shouldValidate
        "Pass if additional tokens (non-NFT) minted"
        validData
        manyTokensCtx

      mapM_
        (\(ctx, str) -> shouldFailWithErr str "Royalities not paid" validData ctx)
        insufficientShareCtxs

validData :: TestData ( 'ForMinting MintAct)
validData = MintingTest redeemer tokens
  where
    tokens = token validOldTokenName (-1) <> token validNewTokenName 1
    redeemer = ChangeOwner TestValues.nft2 TestValues.userTwoPkh

badTokenNameData :: TestData ( 'ForMinting MintAct)
badTokenNameData = MintingTest redeemer tokens
  where
    breakName = TokenName . sha2_256 . unTokenName
    tokens = token validOldTokenName (-1) <> token (breakName validNewTokenName) 1
    redeemer = ChangeOwner TestValues.nft2 TestValues.userTwoPkh

oldTokenNotBurntData :: TestData ( 'ForMinting MintAct)
oldTokenNotBurntData = MintingTest redeemer tokens
  where
    tokens = token validNewTokenName 1
    redeemer = ChangeOwner TestValues.nft2 TestValues.userTwoPkh

validOldTokenName :: TokenName
validOldTokenName = mkTokenName TestValues.nft2

validNewTokenName :: TokenName
validNewTokenName = mkTokenName TestValues.nft3

marketplShareCtx :: ContextBuilder ( 'ForMinting MintAct)
marketplShareCtx =
  paysToOther
    TestValues.marketplValHash
    TestValues.marketplShareVal
    testTokenCurSym

ownerShareCtx :: ContextBuilder ( 'ForMinting MintAct)
ownerShareCtx =
  paysToPubKeyWithDatum
    (unPaymentPubKeyHash TestValues.userOnePkh)
    TestValues.ownerShareVal
    testTokenCurSym

authorShareCtx :: ContextBuilder ( 'ForMinting MintAct)
authorShareCtx =
  paysToPubKeyWithDatum
    (unPaymentPubKeyHash TestValues.authorPkh)
    TestValues.authorShareVal
    testTokenCurSym

validCtx :: ContextBuilder ( 'ForMinting MintAct)
validCtx = marketplShareCtx <> authorShareCtx <> ownerShareCtx

insufficientShareCtxs :: [(ContextBuilder ( 'ForMinting MintAct), String)]
insufficientShareCtxs =
  makeIncompleteContexts
    [ (marketplShareCtx, "Fails when marketplace share is insufficient")
    , (authorShareCtx, "Fails when author share is insufficient")
    , (ownerShareCtx, "Fails when owner share is insufficient")
    ]

manyTokensCtx :: ContextBuilder ( 'ForMinting MintAct)
manyTokensCtx =
  validCtx
    <> mintsValue additionalValue
  where
    additionalValue = Value.singleton (Value.CurrencySymbol "aa") (TokenName "ff") 1

testTokenCurSym :: CurrencySymbol
testTokenCurSym = scriptCurrencySymbol testTokenPolicy

-- test policy
testTokenPolicy :: MintingPolicy
testTokenPolicy =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||go||])
      `PlutusTx.applyCode` ( $$(PlutusTx.compile [||mkPolicy||])
                              `PlutusTx.applyCode` PlutusTx.liftCode TestValues.burnHash
                              `PlutusTx.applyCode` PlutusTx.liftCode Nothing
                              `PlutusTx.applyCode` PlutusTx.liftCode TestValues.collectionNft
                           )
  where
    go = toTestMintingPolicy

shouldFailWithErr ::
  forall (p :: Purpose).
  Typeable p =>
  String ->
  BuiltinString ->
  TestData p ->
  ContextBuilder p ->
  WithScript p ()
shouldFailWithErr name errMsg =
  shouldn'tValidateTracing name (errMsg' `elem`)
  where
    errMsg' = fromBuiltin errMsg
