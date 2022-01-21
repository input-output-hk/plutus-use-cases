module Test.EfficientNFT.Script.TokenChangePrice (test) where

import Ledger (PaymentPubKeyHash (unPaymentPubKeyHash))
import Ledger.Value (TokenName (TokenName, unTokenName))
import Mlabs.EfficientNFT.Token (
  mkPolicy,
 )
import Mlabs.EfficientNFT.Types (
  MintAct (ChangePrice),
  NftId (nftId'price),
 )
import Plutus.V1.Ledger.Ada qualified as Value
import PlutusTx qualified
import PlutusTx.Positive (positive)
import PlutusTx.Prelude hiding (elem, mconcat, mempty, (<>))
import Test.EfficientNFT.Script.Values qualified as TestValues
import Test.Tasty (TestTree)
import Test.Tasty.Plutus.Context (
  ContextBuilder,
  Purpose (ForMinting),
  input,
  signedWith,
  spendsFromPubKey,
  spendsFromPubKeySigned,
 )
import Test.Tasty.Plutus.Script.Unit (
  shouldValidate,
  shouldn'tValidateTracing,
 )
import Test.Tasty.Plutus.TestData (TestData (MintingTest), Tokens (Tokens), burnTokens, mintTokens)
import Test.Tasty.Plutus.TestScript (TestScript, mkTestMintingPolicy, toTestMintingPolicy)
import Test.Tasty.Plutus.WithScript (
  WithScript,
  withTestScript,
 )
import Type.Reflection (Typeable)
import Prelude (String, elem, (<>))

test :: TestTree
test =
  withTestScript "Change price" testTokenPolicy $ do
    shouldValidate "Change price with valid data and context" validData validCtx

    shouldFailWithErr
      "Fail if not signed by owner"
      "Owner must sign the transaction"
      validData
      wrongSignCtx

    shouldFailWithErr
      "Fail if new token not minted"
      "Exactly one new token must be minted and exactly one old burnt"
      newNotMintedData
      validCtx

    shouldFailWithErr
      "Fail if old token not burnt"
      "Exactly one new token must be minted and exactly one old burnt"
      oldNotBurntData
      validCtx

    shouldFailWithErr
      "Fail if wrong amount minted"
      "Exactly one new token must be minted and exactly one old burnt"
      wrongAmtMintedData
      validCtx

    shouldFailWithErr
      "Fail if wrong amount burned"
      "Exactly one new token must be minted and exactly one old burnt"
      wrongAmtBurnedData
      validCtx

    shouldFailWithErr
      "Fail if token with wrong name minted"
      "Exactly one new token must be minted and exactly one old burnt"
      wrongNameMintedData
      validCtx

    shouldFailWithErr
      "Fail if token with wrong name burned"
      "Exactly one new token must be minted and exactly one old burnt"
      wrongNameBurnedData
      validCtx

-- test data
testRedeemer :: MintAct
testRedeemer = ChangePrice TestValues.nft1 newPrice
  where
    newPrice = nftId'price TestValues.newPriceNft1

validData :: TestData ( 'ForMinting MintAct)
validData =
  MintingTest
    testRedeemer
    ( burnTokens (Tokens TestValues.tokenName [positive| 1 |])
        <> mintTokens (Tokens TestValues.newPriceTokenName [positive| 1 |])
    )

newNotMintedData :: TestData ( 'ForMinting MintAct)
newNotMintedData =
  MintingTest
    testRedeemer
    (burnTokens (Tokens TestValues.tokenName [positive| 1 |]))

oldNotBurntData :: TestData ( 'ForMinting MintAct)
oldNotBurntData =
  MintingTest
    testRedeemer
    (burnTokens (Tokens TestValues.newPriceTokenName [positive| 1 |]))

wrongAmtMintedData :: TestData ( 'ForMinting MintAct)
wrongAmtMintedData =
  MintingTest
    testRedeemer
    ( burnTokens (Tokens TestValues.tokenName [positive| 1 |])
        <> mintTokens (Tokens TestValues.newPriceTokenName [positive| 2 |])
    )

wrongAmtBurnedData :: TestData ( 'ForMinting MintAct)
wrongAmtBurnedData =
  MintingTest
    testRedeemer
    ( mintTokens (Tokens TestValues.tokenName [positive| 1 |])
        <> mintTokens (Tokens TestValues.newPriceTokenName [positive| 1 |])
    )

-- test context
validCtx :: ContextBuilder ( 'ForMinting r)
validCtx =
  let pkh = unPaymentPubKeyHash TestValues.authorPkh
   in spendsFromPubKeySigned pkh (Value.lovelaceValueOf 1000000)

wrongNameMintedData :: TestData ( 'ForMinting MintAct)
wrongNameMintedData =
  MintingTest
    testRedeemer
    ( burnTokens (Tokens TestValues.tokenName [positive| 1 |])
        <> mintTokens (Tokens (breakName TestValues.newPriceTokenName) [positive| 1 |])
    )

wrongNameBurnedData :: TestData ( 'ForMinting MintAct)
wrongNameBurnedData =
  MintingTest
    testRedeemer
    ( burnTokens (Tokens (breakName TestValues.tokenName) [positive| 1 |])
        <> mintTokens (Tokens TestValues.newPriceTokenName [positive| 1 |])
    )

wrongSignCtx :: ContextBuilder ( 'ForMinting r)
wrongSignCtx =
  spendsFromPubKey (unPaymentPubKeyHash TestValues.authorPkh) (Value.lovelaceValueOf 1000000)
    <> signedWith (unPaymentPubKeyHash TestValues.otherPkh)

-- test policy
testTokenPolicy :: TestScript ( 'ForMinting MintAct)
testTokenPolicy =
  mkTestMintingPolicy
    ( $$(PlutusTx.compile [||mkPolicy||])
        `PlutusTx.applyCode` PlutusTx.liftCode TestValues.burnHash
        `PlutusTx.applyCode` PlutusTx.liftCode Nothing
        `PlutusTx.applyCode` PlutusTx.liftCode TestValues.collectionNft
    )
    $$(PlutusTx.compile [||toTestMintingPolicy||])

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

breakName :: TokenName -> TokenName
breakName = TokenName . sha2_256 . unTokenName
