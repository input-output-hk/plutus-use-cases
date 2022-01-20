{-# LANGUAGE QuasiQuotes #-}

module Test.EfficientNFT.Script.TokenMint (test) where

import PlutusTx qualified
import PlutusTx.Positive (positive)
import PlutusTx.Prelude hiding (elem, mconcat, pure, (<>))
import Prelude (elem, mconcat, pure, (<>))

import Data.Data (Typeable)
import Data.String (String)
import Ledger (
  TxOutRef (txOutRefId),
  unPaymentPubKeyHash,
 )
import Ledger.Value (TokenName (TokenName), unTokenName)
import Plutus.V1.Ledger.Ada qualified as Ada
import Plutus.V1.Ledger.Value qualified as Value
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.Plutus.Context (
  ContextBuilder,
  Purpose (ForMinting),
  mintsValue,
  paysToOther,
  paysToPubKey,
  spendsFromPubKey,
 )
import Test.Tasty.Plutus.Options (TestTxId (TestTxId))
import Test.Tasty.Plutus.Script.Unit (
  shouldValidate,
  shouldn'tValidateTracing,
 )
import Test.Tasty.Plutus.TestData (
  TestData (MintingTest),
  Tokens (Tokens),
  mintTokens,
 )
import Test.Tasty.Plutus.TestScript (TestScript, mkTestMintingPolicy, toTestMintingPolicy)
import Test.Tasty.Plutus.WithScript (WithScript, withTestScript)

import Mlabs.EfficientNFT.Token (mkPolicy)
import Mlabs.EfficientNFT.Types (MintAct (MintToken))
import Test.EfficientNFT.Script.Values qualified as TestValues

test :: TestTree
test =
  testGroup
    "Minting"
    $ pure $
      localOption (TestTxId $ txOutRefId TestValues.mintTxOutRef) $
        withTestScript "Token policy" testTokenPolicy $ do
          shouldValidate "Valid data and context" validData validCtx

          shouldFailWithErr
            "Fail if token has wrong name"
            "Exactly one NFT must be minted"
            badTokenNameData
            validCtx

          shouldFailWithErr
            "Fail if minted amount not 1"
            "Exactly one NFT must be minted"
            wrongNftQuantityData
            validCtx

          shouldValidate
            "Pass if additional tokens (non-NFT) minted"
            validData
            manyTokensCtx

validData :: TestData ( 'ForMinting MintAct)
validData =
  MintingTest
    redeemer
    (mintTokens (Tokens TestValues.tokenName [positive| 1 |]))
  where
    redeemer = MintToken TestValues.nft1

wrongNftQuantityData :: TestData ( 'ForMinting MintAct)
wrongNftQuantityData =
  MintingTest
    redeemer
    (mintTokens (Tokens TestValues.tokenName [positive| 2 |]))
  where
    redeemer = MintToken TestValues.nft1

badTokenNameData :: TestData ( 'ForMinting MintAct)
badTokenNameData =
  MintingTest
    redeemer
    (mintTokens badTokens)
  where
    breakName = TokenName . sha2_256 . unTokenName
    badTokens = Tokens (breakName TestValues.tokenName) [positive| 1 |]
    redeemer = MintToken TestValues.nft1

-- test context
validCtx :: ContextBuilder ( 'ForMinting MintAct)
validCtx =
  mconcat
    [ spendsFromPubKey (unPaymentPubKeyHash TestValues.authorPkh) (Ada.lovelaceValueOf 1000000)
    , paysToOther TestValues.burnHash (Value.assetClassValue TestValues.collectionNft 1) ()
    ]

manyTokensCtx :: ContextBuilder ( 'ForMinting MintAct)
manyTokensCtx =
  validCtx
    <> mintsValue additionalValue
  where
    additionalValue = Value.singleton (Value.CurrencySymbol "aa") (TokenName "ff") 1

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
