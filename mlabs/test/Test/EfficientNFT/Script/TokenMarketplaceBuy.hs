module Test.EfficientNFT.Script.TokenMarketplaceBuy (test) where

import Prelude

import Ledger (CurrencySymbol, minAdaTxOut, unPaymentPubKeyHash)
import Ledger.Value (CurrencySymbol (CurrencySymbol), TokenName (TokenName, unTokenName), singleton)
import Plutus.V1.Ledger.Ada (toValue)
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))
import PlutusTx.Builtins (BuiltinData)
import Test.Tasty (TestTree)
import Test.Tasty.Plutus.Context (ContextBuilder, Purpose (ForSpending), mintsValue, paysToOther, paysToPubKeyWithDatum, paysToSelf)
import Test.Tasty.Plutus.Script.Unit (shouldValidate, shouldn'tValidate)
import Test.Tasty.Plutus.TestData (TestData (SpendingTest))
import Test.Tasty.Plutus.WithScript (withTestScript)

import Mlabs.EfficientNFT.Token (mkTokenName)
import Test.EfficientNFT.Script.Values qualified as TestValues

test :: TestTree
test = withTestScript "Buy" TestValues.testMarketplaceScript $ do
  shouldValidate "Buy with valid data and context" validData validCtx

  shouldValidate "Buy multiple tokens" multipleTokensData multipleTokensCtx

  shouldn'tValidate "Fail when not minting" multipleTokensData noMintCtx

dtm :: BuiltinData
dtm = toBuiltinData ()

mockSgCs :: CurrencySymbol
mockSgCs = CurrencySymbol "ff"

secondCs :: CurrencySymbol
secondCs = CurrencySymbol "aa"

secondTn :: TokenName
secondTn = TokenName "foo"

validOldTokenName :: TokenName
validOldTokenName = mkTokenName TestValues.nft2

validNewTokenName :: TokenName
validNewTokenName = mkTokenName TestValues.nft3

validData :: TestData ( 'ForSpending BuiltinData BuiltinData)
validData = SpendingTest dtm redeemer val
  where
    redeemer = dtm
    val =
      mconcat
        [ toValue minAdaTxOut
        , singleton mockSgCs validOldTokenName 1
        ]

multipleTokensData :: TestData ( 'ForSpending BuiltinData BuiltinData)
multipleTokensData = SpendingTest dtm redeemer val
  where
    redeemer = dtm
    val =
      mconcat
        [ toValue minAdaTxOut
        , singleton mockSgCs validOldTokenName 1
        , singleton mockSgCs secondTn 1
        , singleton secondCs TestValues.tokenName 1
        ]

validCtx :: ContextBuilder ( 'ForSpending BuiltinData BuiltinData)
validCtx =
  mconcat
    [ mintsValue $
        mconcat
          [ singleton mockSgCs validOldTokenName (negate 1)
          , singleton mockSgCs validNewTokenName 1
          ]
    , paysToSelf
        ( mconcat
            [ singleton mockSgCs validNewTokenName 1
            , toValue minAdaTxOut
            ]
        )
        dtm
    , royalitiesCtx
    ]

multipleTokensCtx :: ContextBuilder ( 'ForSpending BuiltinData BuiltinData)
multipleTokensCtx =
  mconcat
    [ mintsValue $
        mconcat
          [ singleton mockSgCs validOldTokenName (negate 1)
          , singleton mockSgCs validNewTokenName 1
          , singleton mockSgCs secondTn (negate 1)
          , singleton mockSgCs (TokenName (unTokenName secondTn <> "x")) 1
          , singleton secondCs TestValues.tokenName (negate 1)
          , singleton secondCs (TokenName (unTokenName TestValues.tokenName <> "x")) 1
          ]
    , paysToSelf
        ( mconcat
            [ singleton mockSgCs validNewTokenName 1
            , singleton mockSgCs (TokenName (unTokenName secondTn <> "x")) 1
            , singleton secondCs (TokenName (unTokenName TestValues.tokenName <> "x")) 1
            , toValue minAdaTxOut
            ]
        )
        dtm
    , royalitiesCtx
    ]

noMintCtx :: ContextBuilder ( 'ForSpending BuiltinData BuiltinData)
noMintCtx =
  mconcat
    [ mintsValue $
        mconcat
          [ singleton mockSgCs validOldTokenName 1
          , toValue minAdaTxOut
          , singleton mockSgCs secondTn (negate 1)
          , singleton mockSgCs (TokenName (unTokenName secondTn <> "x")) 1
          ]
    , paysToSelf
        ( mconcat
            [ singleton mockSgCs validNewTokenName 1
            , singleton mockSgCs (TokenName (unTokenName secondTn <> "x")) 1
            , singleton secondCs TestValues.tokenName 1
            , toValue minAdaTxOut
            ]
        )
        dtm
    , royalitiesCtx
    ]

royalitiesCtx :: ContextBuilder p
royalitiesCtx =
  mconcat
    [ paysToOther
        TestValues.daoValHash
        TestValues.daoShareVal
        (mockSgCs, validOldTokenName)
    , paysToPubKeyWithDatum
        (unPaymentPubKeyHash TestValues.userOnePkh)
        TestValues.ownerShareVal
        (mockSgCs, validOldTokenName)
    , paysToPubKeyWithDatum
        (unPaymentPubKeyHash TestValues.authorPkh)
        TestValues.authorShareVal
        (mockSgCs, validOldTokenName)
    ]
