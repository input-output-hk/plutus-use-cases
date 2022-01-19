module Test.EfficientNFT.Script.TokenMint (test) where

import PlutusTx qualified
import PlutusTx.Prelude hiding (elem, mconcat, pure, (<>))
import Prelude (elem, mconcat, pure, (<>))

import Data.Data (Typeable)
import Data.String (String)
import Ledger (
  MintingPolicy,
  TxOutRef (txOutRefId),
  mkMintingPolicyScript,
  unPaymentPubKeyHash,
 )
import Ledger.Value (TokenName (TokenName), unTokenName)
import Plutus.V1.Ledger.Ada qualified as Ada
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx.AssocMap qualified as Map
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.Plutus.Context (
  ContextBuilder,
  ExternalType (PubKeyType, ScriptType),
  Input (Input),
  Output (Output),
  Purpose (ForMinting),
  input,
  mintsValue,
  output,
 )
import Test.Tasty.Plutus.Options (TestTxId (TestTxId))
import Test.Tasty.Plutus.Script.Unit (
  shouldValidate,
  shouldn'tValidateTracing,
 )
import Test.Tasty.Plutus.TestData (
  TestData (MintingTest),
  Tokens (Tokens),
  token,
 )
import Test.Tasty.Plutus.WithScript (
  WithScript,
  toTestMintingPolicy,
  withMintingPolicy,
 )

import Mlabs.EfficientNFT.Token (mkPolicy)
import Mlabs.EfficientNFT.Types (MintAct (MintToken))
import Test.EfficientNFT.Script.Values qualified as TestValues

test :: TestTree
test =
  testGroup
    "Minting"
    $ pure $
      localOption (TestTxId $ txOutRefId TestValues.mintTxOutRef) $
        withMintingPolicy "Token policy" testTokenPolicy $ do
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

          shouldFailWithErr
            "Fail if no NFT minted"
            "Exactly one NFT must be minted"
            noMintedTokensData
            validCtx

-- test data
correctTokens :: Tokens
correctTokens = token TestValues.tokenName 1

validData :: TestData ( 'ForMinting MintAct)
validData =
  MintingTest
    redeemer
    correctTokens
  where
    redeemer = MintToken TestValues.nft1

wrongNftQuantityData :: TestData ( 'ForMinting MintAct)
wrongNftQuantityData =
  MintingTest
    redeemer
    (correctTokens <> correctTokens)
  where
    redeemer = MintToken TestValues.nft1

noMintedTokensData :: TestData ( 'ForMinting MintAct)
noMintedTokensData =
  MintingTest
    redeemer
    (Tokens Map.empty)
  where
    redeemer = MintToken TestValues.nft1

badTokenNameData :: TestData ( 'ForMinting MintAct)
badTokenNameData =
  MintingTest
    redeemer
    badTokens
  where
    breakName = TokenName . sha2_256 . unTokenName
    badTokens = token (breakName TestValues.tokenName) 1
    redeemer = MintToken TestValues.nft1

-- test context
validCtx :: ContextBuilder ( 'ForMinting r)
validCtx =
  mconcat
    [ input $ Input (PubKeyType (unPaymentPubKeyHash TestValues.authorPkh) Nothing) (Ada.lovelaceValueOf 1000000)
    , output $ Output (ScriptType TestValues.burnHash (PlutusTx.toBuiltinData ())) (Value.assetClassValue TestValues.collectionNft 1)
    ]

manyTokensCtx :: ContextBuilder ( 'ForMinting r)
manyTokensCtx =
  validCtx
    <> mintsValue additionalValue
  where
    additionalValue = Value.singleton (Value.CurrencySymbol "aa") (TokenName "ff") 1

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
