{-# LANGUAGE QuasiQuotes #-}

module Test.EfficientNFT.Script.TokenChangeOwner (test) where

import Ledger (PaymentPubKeyHash (unPaymentPubKeyHash))
import Ledger.Value (CurrencySymbol, TokenName (TokenName, unTokenName))
import Ledger.Value qualified as Value
import PlutusTx.Positive (Positive, positive)

import Data.Data (Typeable)
import Data.List.NonEmpty (NonEmpty)
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
import Test.Tasty.Plutus.Script.Property (scriptPropertyFail)
import Test.Tasty.Plutus.Script.Unit (shouldValidate, shouldn'tValidateTracing)
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

import Mlabs.EfficientNFT.Token (mkTokenName)
import Mlabs.EfficientNFT.Types (MintAct (ChangeOwner))

import Test.EfficientNFT.Script.Values qualified as TestValues

test :: TestTree
test =
  localOption (TestCurrencySymbol testTokenCurSym) $
    withTestScript "Token change owner" TestValues.testTokenPolicy $
      do
        shouldValidate "valid buy" validData validCtx
        shouldFailWithErr
          "Fail if token has wrong name"
          "Exactly one new token must be minted and exactly one old burnt"
          badTokenNameData
          validCtx

        shouldFailWithErr
          "Fail if old token is not burnt"
          "Exactly one new token must be minted and exactly one old burnt"
          oldTokenNotBurntData
          validCtx

        shouldFailWithErr
          "Fail if new token is not minted"
          "Exactly one new token must be minted and exactly one old burnt"
          newTokenNotMintedData
          validCtx

        scriptPropertyFail "Should mint and burn exactly 1 token" $
          GenForMinting fromArbitrary oneTokenMintAndBurn

        shouldValidate
          "Pass if additional tokens (non-NFT) minted"
          validData
          manyTokensCtx

        mapM_
          (\(ctx, str) -> shouldFailWithErr str "Royalities not paid" validData ctx)
          insufficientShareCtxs

validData :: TestData ( 'ForMinting MintAct)
validData = MintingTest redeemer tasks
  where
    tasks =
      burnTokens (Tokens validOldTokenName [positive| 1 |])
        <> mintTokens (Tokens validNewTokenName [positive| 1 |])
    redeemer = ChangeOwner TestValues.nft2 TestValues.userTwoPkh

badTokenNameData :: TestData ( 'ForMinting MintAct)
badTokenNameData = MintingTest redeemer tasks
  where
    breakName = TokenName . sha2_256 . unTokenName
    tasks =
      burnTokens (Tokens validOldTokenName [positive| 1 |])
        <> mintTokens (Tokens (breakName validNewTokenName) [positive| 1 |])
    redeemer = ChangeOwner TestValues.nft2 TestValues.userTwoPkh

oldTokenNotBurntData :: TestData ( 'ForMinting MintAct)
oldTokenNotBurntData = MintingTest redeemer tasks
  where
    tasks = mintTokens (Tokens validNewTokenName [positive| 1 |])
    redeemer = ChangeOwner TestValues.nft2 TestValues.userTwoPkh

newTokenNotMintedData :: TestData ( 'ForMinting MintAct)
newTokenNotMintedData = MintingTest redeemer tasks
  where
    tasks = burnTokens (Tokens validOldTokenName [positive| 1 |])
    redeemer = ChangeOwner TestValues.nft2 TestValues.userTwoPkh

validOldTokenName :: TokenName
validOldTokenName = mkTokenName TestValues.nft2

validNewTokenName :: TokenName
validNewTokenName = mkTokenName TestValues.nft3

daoShareCtx :: ContextBuilder ( 'ForMinting MintAct)
daoShareCtx =
  paysToOther
    TestValues.daoValHash
    TestValues.daoShareVal
    (testTokenCurSym, validOldTokenName)

ownerShareCtx :: ContextBuilder ( 'ForMinting MintAct)
ownerShareCtx =
  paysToPubKeyWithDatum
    (unPaymentPubKeyHash TestValues.userOnePkh)
    TestValues.ownerShareVal
    (testTokenCurSym, validOldTokenName)

authorShareCtx :: ContextBuilder ( 'ForMinting MintAct)
authorShareCtx =
  paysToPubKeyWithDatum
    (unPaymentPubKeyHash TestValues.authorPkh)
    TestValues.authorShareVal
    (testTokenCurSym, validOldTokenName)

validCtx :: ContextBuilder ( 'ForMinting MintAct)
validCtx = daoShareCtx <> authorShareCtx <> ownerShareCtx

insufficientShareCtxs :: [(ContextBuilder ( 'ForMinting MintAct), String)]
insufficientShareCtxs =
  makeIncompleteContexts
    [ (daoShareCtx, "Fails when marketplace share is insufficient")
    , (authorShareCtx, "Fails when author share is insufficient")
    , (ownerShareCtx, "Fails when owner share is insufficient")
    ]

manyTokensCtx :: ContextBuilder ( 'ForMinting MintAct)
manyTokensCtx =
  validCtx
    <> mintsValue additionalValue
  where
    additionalValue = Value.singleton (Value.CurrencySymbol "aa") (TokenName "ff") 1

-- | Creates TestItems with an arbitrary key used in Redeemer
oneTokenMintAndBurn :: (Positive, Positive) -> TestItems ( 'ForMinting MintAct)
oneTokenMintAndBurn (mintAmt, burnAmt) =
  ItemsForMinting
    { mpRedeemer = redeemer
    , mpTasks = tasks
    , mpCB = validCtx
    , mpOutcome = out
    }
  where
    mintAmt' = mintAmt + [positive| 1 |]
    burnAmt' = burnAmt + [positive| 1 |]

    toksMint = Tokens validNewTokenName mintAmt'
    toksBurn = Tokens validOldTokenName burnAmt'

    redeemer = ChangeOwner TestValues.nft2 TestValues.userTwoPkh

    tasks :: NonEmpty MintingPolicyTask
    tasks = mintTokens toksMint <> burnTokens toksBurn

    out :: Outcome
    out = passIf $ fromEnum mintAmt' == 1 && fromEnum burnAmt' == 1

testTokenCurSym :: CurrencySymbol
testTokenCurSym = "aabbcc"

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
