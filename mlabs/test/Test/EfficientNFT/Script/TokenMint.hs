module Test.EfficientNFT.Script.TokenMint (test) where

import Ledger (
  MintingPolicy,
  PaymentPubKeyHash (PaymentPubKeyHash, unPaymentPubKeyHash),
  PubKeyHash (PubKeyHash, getPubKeyHash),
  TxId (TxId),
  TxOutRef (txOutRefId),
  mkMintingPolicyScript,
 )
import Ledger.Value (TokenName (TokenName, unTokenName))
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Ada qualified as Value
import PlutusTx qualified
import PlutusTx.AssocMap qualified as Map

import PlutusTx.Prelude hiding (elem, mconcat, mempty, (<>))
import Prelude (String, elem, (<>))

import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.Plutus.Context
import Test.Tasty.Plutus.Options (TestTxId (TestTxId))
import Test.Tasty.Plutus.Script.Unit (shouldValidate, shouldn'tValidate, shouldn'tValidateTracing)
import Test.Tasty.Plutus.TestData (TestData (MintingTest), Tokens (Tokens), token)
import Test.Tasty.Plutus.WithScript (WithScript, toTestMintingPolicy, withMintingPolicy)

import Type.Reflection (Typeable)

import Mlabs.EfficientNFT.Token (mkPolicy)
import Mlabs.EfficientNFT.Types (
  MintAct (MintToken),
  OwnerData (OwnerData, odOwnerPkh),
 )

import Test.EfficientNFT.Script.Values qualified as TestValues

test :: TestTree
test =
  testGroup
    "Minting"
    [ wrongUtxo
    , okMint
    ]
  where
    wrongUtxo = localOption (TestTxId $ TxId "ff") $
      withMintingPolicy "UTXO parametrization test" testTokenPolicy $ do
        shouldn'tValidate "fails with wrong UTXO consumed" validData validCtx

    okMint = localOption (TestTxId $ txOutRefId TestValues.mintTxOutRef) $
      withMintingPolicy "Token policy" testTokenPolicy $ do
        shouldValidate "valid data and context" validData validCtx
        -- maybe, property test here will be better (`plutus-extra` update required)
        shouldFailWithErr
          "fail if author is not the owner"
          "The author must be the first owner of the NFT"
          (breakAuthorPkh validData)
          validCtx

        shouldFailWithErr
          "fail if token has wrong name"
          "Token name must be the hash of the owner pkh and the price"
          badTokenNameData
          validCtx

        shouldFailWithErr
          "fail if minted amount not 1"
          "Exactly one NFT must be minted"
          wrongNftQuantityData
          validCtx

        shouldFailWithErr
          "fail if additional tokens minted"
          "Exactly one NFT must be minted"
          validData
          manyTokensCtx

        shouldFailWithErr
          "fail if no NFT minted"
          "Exactly one NFT must be minted"
          noMintedTokensData
          validCtx

-- test data
testRedeemer :: OwnerData
testRedeemer = OwnerData TestValues.authorPkh TestValues.nftPrice

correctTokens :: Tokens
correctTokens = token TestValues.tokenName 1

validData :: TestData ( 'ForMinting MintAct)
validData =
  MintingTest
    (MintToken testRedeemer)
    correctTokens

wrongNftQuantityData :: TestData ( 'ForMinting MintAct)
wrongNftQuantityData =
  MintingTest
    (MintToken testRedeemer)
    (correctTokens <> correctTokens)

breakAuthorPkh :: TestData ( 'ForMinting MintAct) -> TestData ( 'ForMinting MintAct)
breakAuthorPkh (MintingTest rmr toks) =
  let Just (MintToken ownerData) = PlutusTx.fromData . PlutusTx.toData $ rmr
      brokenPkh =
        PaymentPubKeyHash . PubKeyHash
          . sha2_256
          . getPubKeyHash
          . unPaymentPubKeyHash
          . odOwnerPkh
          $ ownerData
      brokenData = ownerData {odOwnerPkh = brokenPkh}
   in MintingTest (MintToken brokenData) toks

noMintedTokensData :: TestData ( 'ForMinting MintAct)
noMintedTokensData =
  MintingTest
    (MintToken testRedeemer)
    (Tokens Map.empty)

badTokenNameData :: TestData ( 'ForMinting MintAct)
badTokenNameData =
  MintingTest
    (MintToken testRedeemer)
    badTokens
  where
    breakName = TokenName . sha2_256 . unTokenName
    badTokens = token (breakName TestValues.tokenName) 1

-- test context
validCtx :: ContextBuilder ( 'ForMinting MintAct)
validCtx =
  input $
    Input
      (PubKeyType (unPaymentPubKeyHash TestValues.authorPkh) Nothing)
      (Value.lovelaceValueOf 1000000)

manyTokensCtx :: ContextBuilder ( 'ForMinting MintAct)
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
