module Test.EfficientNFT.Script.TokenChangePrice (test) where

import Ledger
  ( MintingPolicy,
    PaymentPubKeyHash (unPaymentPubKeyHash),
    mkMintingPolicyScript,
  )
import Ledger.Value (TokenName (TokenName, unTokenName))
import Mlabs.EfficientNFT.Token
  ( mkPolicy,
  )
import Mlabs.EfficientNFT.Types
  ( MintAct (ChangePrice),
    OwnerData (OwnerData),
  )
import Plutus.V1.Ledger.Ada qualified as Value
import PlutusTx qualified
import PlutusTx.Prelude hiding (elem, mconcat, mempty, (<>))
import Test.EfficientNFT.Script.Values qualified as TestValues
import Test.Tasty (TestTree)
import Test.Tasty.Plutus.Context
  ( ContextBuilder,
    ExternalType (PubKeyType),
    Input (Input),
    Purpose (ForMinting),
    input,
    signedWith,
  )
import Test.Tasty.Plutus.Script.Unit
  ( shouldValidate,
    shouldn'tValidate,
    shouldn'tValidateTracing,
  )
import Test.Tasty.Plutus.TestData
  ( TestData (MintingTest),
    token,
  )
import Test.Tasty.Plutus.WithScript
  ( WithScript,
    toTestMintingPolicy,
    withMintingPolicy,
  )
import Type.Reflection (Typeable)
import Prelude (String, elem, (<>))

test :: TestTree
test =
  withMintingPolicy "Change price" testTokenPolicy $ do
    shouldValidate "valid data and context" validData validCtx

    shouldFailWithErr
      "fail if not signed by owner"
      "Owner must sign the transaction"
      validData
      wrongSignCtx

    shouldFailWithErr
      "fail if minted token has wrong name"
      "Token name must be the hash of the owner pkh and the price"
      badTokenNameData
      validCtx

    shouldFailWithErr
      "fail if old token not burnt"
      "Old version must be burnt when reminting"
      oldNotBurntData
      validCtx

    -- todo: it's better to check exact trace message
    shouldn'tValidate
      "fail if more than one new token minted"
      tooManyMintedData
      validCtx

    -- todo: it's better to check exact trace message
    shouldn'tValidate
      "fail if no new token minted"
      newNotMintedData
      validCtx

-- test data
testRedeemer :: MintAct
testRedeemer = ChangePrice ownerData TestValues.newPrice
  where
    ownerData = OwnerData TestValues.authorPkh TestValues.nftPrice

validData :: TestData ('ForMinting MintAct)
validData =
  MintingTest
    testRedeemer
    ( token TestValues.tokenName (-1)
        <> token TestValues.newPriceTokenName 1
    )

badTokenNameData :: TestData ('ForMinting MintAct)
badTokenNameData =
  MintingTest
    testRedeemer
    withBadNewToken
  where
    breakName = TokenName . sha2_256 . unTokenName
    withBadNewToken =
      token TestValues.tokenName (-1)
        <> token (breakName TestValues.newPriceTokenName) 1

oldNotBurntData :: TestData ('ForMinting MintAct)
oldNotBurntData =
  MintingTest
    testRedeemer
    (token TestValues.newPriceTokenName 1)

tooManyMintedData :: TestData ('ForMinting MintAct)
tooManyMintedData =
  MintingTest
    testRedeemer
    ( token TestValues.tokenName (-1)
        <> token TestValues.newPriceTokenName 2
    )

newNotMintedData :: TestData ('ForMinting MintAct)
newNotMintedData =
  MintingTest
    testRedeemer
    (token TestValues.tokenName (-1))

-- test context
validCtx :: ContextBuilder ('ForMinting r)
validCtx =
  let pkh = unPaymentPubKeyHash TestValues.authorPkh
   in input
        ( Input
            (PubKeyType pkh)
            (Value.lovelaceValueOf 1000000)
        )
        <> signedWith pkh

wrongSignCtx :: ContextBuilder ('ForMinting r)
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
                               `PlutusTx.applyCode` PlutusTx.liftCode oref'
                               `PlutusTx.applyCode` PlutusTx.liftCode authorPkh'
                               `PlutusTx.applyCode` PlutusTx.liftCode royalty'
                               `PlutusTx.applyCode` PlutusTx.liftCode platformCfg'
                               `PlutusTx.applyCode` PlutusTx.liftCode contentHash'
                           )
  where
    go = toTestMintingPolicy
    oref' = TestValues.mintTxOutRef
    authorPkh' = TestValues.authorPkh
    royalty' = toEnum 3
    platformCfg' = TestValues.platformCfg
    contentHash' = TestValues.contentHash

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
