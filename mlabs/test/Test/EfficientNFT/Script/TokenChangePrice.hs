module Test.EfficientNFT.Script.TokenChangePrice (test) where

import Ledger (
  MintingPolicy,
  PaymentPubKeyHash (unPaymentPubKeyHash),
  mkMintingPolicyScript,
 )
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
import PlutusTx.Prelude hiding (elem, mconcat, mempty, (<>))
import Test.EfficientNFT.Script.Values qualified as TestValues
import Test.Tasty (TestTree)
import Test.Tasty.Plutus.Context (
  ContextBuilder,
  ExternalType (PubKeyType),
  Input (Input),
  Purpose (ForMinting),
  input,
  signedWith,
 )
import Test.Tasty.Plutus.Script.Unit (
  shouldValidate,
  shouldn'tValidateTracing,
 )
import Test.Tasty.Plutus.TestData (
  TestData (MintingTest),
  token,
 )
import Test.Tasty.Plutus.WithScript (
  WithScript,
  toTestMintingPolicy,
  withMintingPolicy,
 )
import Type.Reflection (Typeable)
import Prelude (String, elem, (<>))

test :: TestTree
test =
  withMintingPolicy "Change price" testTokenPolicy $ do
    shouldValidate "Change price with valid data and context" validData validCtx

    shouldFailWithErr
      "Fail if not signed by owner"
      "Owner must sign the transaction"
      validData
      wrongSignCtx

    shouldFailWithErr
      "Fail if new token not minted"
      "Invalid reminting: wrong tokens amount"
      newNotMintedData
      validCtx

    shouldFailWithErr
      "Fail if old token not burnt"
      "Invalid reminting: wrong tokens amount"
      oldNotBurntData
      validCtx

    shouldFailWithErr
      "Fail if wrong amount minted"
      "Invalid reminting: Exactly 1 new token should be minted"
      wrongAmtMintedData
      validCtx

    shouldFailWithErr
      "Fail if wrong amount burned"
      "Invalid reminting: Exactly 1 old token should be burned"
      wrongAmtBurnedData
      validCtx

    shouldFailWithErr
      "Fail if token with wrong name minted"
      "Invalid reminting: Exactly 1 new token should be minted"
      wrongNameMintedData
      validCtx

    shouldFailWithErr
      "Fail if token with wrong name burned"
      "Invalid reminting: Exactly 1 old token should be burned"
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
    ( token TestValues.tokenName (-1)
        <> token TestValues.newPriceTokenName 1
    )

newNotMintedData :: TestData ( 'ForMinting MintAct)
newNotMintedData =
  MintingTest
    testRedeemer
    (token TestValues.tokenName (-1))

oldNotBurntData :: TestData ( 'ForMinting MintAct)
oldNotBurntData =
  MintingTest
    testRedeemer
    (token TestValues.newPriceTokenName 1)

wrongAmtMintedData :: TestData ( 'ForMinting MintAct)
wrongAmtMintedData =
  MintingTest
    testRedeemer
    ( token TestValues.tokenName (-1)
        <> token TestValues.newPriceTokenName 2
    )

wrongAmtBurnedData :: TestData ( 'ForMinting MintAct)
wrongAmtBurnedData =
  MintingTest
    testRedeemer
    ( token TestValues.tokenName 1
        <> token TestValues.newPriceTokenName 1
    )

-- test context
validCtx :: ContextBuilder ( 'ForMinting r)
validCtx =
  let pkh = unPaymentPubKeyHash TestValues.authorPkh
   in input
        ( Input
            (PubKeyType pkh)
            (Value.lovelaceValueOf 1000000)
        )
        <> signedWith pkh

wrongNameMintedData :: TestData ( 'ForMinting MintAct)
wrongNameMintedData =
  MintingTest
    testRedeemer
    ( token TestValues.tokenName (-1)
        <> token (breakName TestValues.newPriceTokenName) 1
    )

wrongNameBurnedData :: TestData ( 'ForMinting MintAct)
wrongNameBurnedData =
  MintingTest
    testRedeemer
    ( token (breakName TestValues.tokenName) (-1)
        <> token TestValues.newPriceTokenName 1
    )

wrongSignCtx :: ContextBuilder ( 'ForMinting r)
wrongSignCtx =
  input
    ( Input
        (PubKeyType (unPaymentPubKeyHash TestValues.authorPkh))
        (Value.lovelaceValueOf 1000000)
    )
    <> signedWith (unPaymentPubKeyHash TestValues.otherPkh)

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

breakName :: TokenName -> TokenName
breakName = TokenName . sha2_256 . unTokenName
