module Test.EfficientNFT.Script.TokenMint (test) where

import Ledger (
  MintingPolicy,
  PubKeyHash (PubKeyHash, getPubKeyHash),
  TxId (TxId),
  TxOutRef (txOutRefId),
  mkMintingPolicyScript,
 )
import Ledger.Value (TokenName (TokenName, unTokenName))
import Plutus.V1.Ledger.Ada qualified as Value
import PlutusTx qualified

import PlutusTx.Prelude hiding (elem, mconcat, mempty, (<>))
import Prelude (String, elem, (<>))

import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.Plutus.Context
import Test.Tasty.Plutus.Script.Unit

import Type.Reflection (Typeable)

import Mlabs.EfficientNFT.Types (
  MintAct (MintToken),
  OwnerData (OwnerData, odOwnerPkh),
 )

import Mlabs.EfficientNFT.Token (
  mkPolicy,
 )

import Test.EfficientNFT.Script.Values qualified as TestValues

-- import Test.QuickCheck qualified as QC
-- import Test.QuickCheck.Plutus.Instances ()

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
          validData
          wrongNftNameCtx

        shouldFailWithErr
          "fail if minted amount not 1"
          "Exactly one NFT must be minted"
          validData
          wrongNftQuantityCtx

        shouldFailWithErr
          "fail if additional tokens minted"
          "Exactly one NFT must be minted"
          validData
          manyTokensCtx

        shouldFailWithErr
          "fail if no NFT minted"
          "Exactly one NFT must be minted"
          validData
          noTokensCtx

-- test data
validData :: TestData 'ForMinting
validData = MintingTest redeemer
  where
    redeemer = MintToken $ OwnerData TestValues.authorPkh TestValues.nftPrice

breakAuthorPkh :: TestData 'ForMinting -> TestData 'ForMinting
breakAuthorPkh (MintingTest rmr) =
  let Just (MintToken ownerData) = PlutusTx.fromData . PlutusTx.toData $ rmr
      brokenPkh = PubKeyHash . sha2_256 . getPubKeyHash .odOwnerPkh $ ownerData
      brokenData = ownerData {odOwnerPkh = brokenPkh}
   in MintingTest (MintToken brokenData)

-- test contexts
baseCtx :: ContextBuilder 'ForMinting
baseCtx = input $ Input (PubKeyType TestValues.authorPkh) (Value.lovelaceValueOf 1000000)

validCtx :: ContextBuilder 'ForMinting
validCtx = baseCtx <> mintsWithSelf TestValues.tokenName 1

wrongNftQuantityCtx :: ContextBuilder 'ForMinting
wrongNftQuantityCtx = baseCtx <> mintsWithSelf TestValues.tokenName 2

wrongNftNameCtx :: ContextBuilder 'ForMinting
wrongNftNameCtx = baseCtx <> mintsWithSelf (breakName TestValues.tokenName) 1
  where
    breakName = TokenName . sha2_256 . unTokenName

manyTokensCtx :: ContextBuilder 'ForMinting
manyTokensCtx =
  validCtx
    <> mintsWithSelf (TokenName "ff") 1

noTokensCtx :: ContextBuilder 'ForMinting
noTokensCtx = input $ Input (PubKeyType TestValues.authorPkh) (Value.lovelaceValueOf 1000000)

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
