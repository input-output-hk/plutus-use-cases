module Test.EfficientNFT.Script.TokenMarketplaceSetPrice (test) where

import Prelude

import Ledger (CurrencySymbol, minAdaTxOut)
import Ledger.Value (CurrencySymbol (CurrencySymbol), TokenName (TokenName), singleton, unTokenName)
import Plutus.V1.Ledger.Ada (toValue)
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))
import PlutusTx.Builtins (BuiltinData)
import Test.Tasty (TestTree)
import Test.Tasty.Plutus.Context (ContextBuilder, Purpose (ForSpending), mintsValue, paysToSelf)
import Test.Tasty.Plutus.Script.Unit (shouldValidate, shouldn'tValidate)
import Test.Tasty.Plutus.TestData (TestData (SpendingTest))
import Test.Tasty.Plutus.WithScript (withTestScript)

import Test.EfficientNFT.Script.Values qualified as TestValues

test :: TestTree
test = withTestScript "Change price" TestValues.testMarketplaceScript $ do
  shouldValidate "Change price with valid data and context" validData validCtx

  shouldValidate "Change price of multiple tokens" multipleTokensData multipleTokensCtx

  shouldn'tValidate "Fail when not minting" multipleTokensData noMintCtx

dtm :: BuiltinData
dtm = toBuiltinData ()

mockSgCs :: CurrencySymbol
mockSgCs = CurrencySymbol "ff"

secondCs :: CurrencySymbol
secondCs = CurrencySymbol "aa"

secondTn :: TokenName
secondTn = TokenName "foo"

validData :: TestData ( 'ForSpending BuiltinData BuiltinData)
validData = SpendingTest dtm redeemer val
  where
    redeemer = dtm
    val =
      mconcat
        [ toValue minAdaTxOut
        , singleton mockSgCs TestValues.tokenName 1
        ]

multipleTokensData :: TestData ( 'ForSpending BuiltinData BuiltinData)
multipleTokensData = SpendingTest dtm redeemer val
  where
    redeemer = dtm
    val =
      mconcat
        [ toValue minAdaTxOut
        , singleton mockSgCs TestValues.tokenName 1
        , singleton mockSgCs secondTn 1
        , singleton secondCs TestValues.tokenName 1
        ]

validCtx :: ContextBuilder ( 'ForSpending BuiltinData BuiltinData)
validCtx =
  mconcat
    [ mintsValue $
        mconcat
          [ singleton mockSgCs TestValues.tokenName (negate 1)
          , singleton mockSgCs TestValues.newPriceTokenName 1
          ]
    , paysToSelf
        ( mconcat
            [ singleton mockSgCs TestValues.newPriceTokenName 1
            , toValue minAdaTxOut
            ]
        )
        dtm
    ]

multipleTokensCtx :: ContextBuilder ( 'ForSpending BuiltinData BuiltinData)
multipleTokensCtx =
  mconcat
    [ mintsValue $
        mconcat
          [ singleton mockSgCs TestValues.tokenName (negate 1)
          , singleton mockSgCs TestValues.newPriceTokenName 1
          , singleton mockSgCs secondTn (negate 1)
          , singleton mockSgCs (TokenName (unTokenName secondTn <> "x")) 1
          , singleton secondCs TestValues.tokenName (negate 1)
          , singleton secondCs (TokenName (unTokenName TestValues.tokenName <> "x")) 1
          ]
    , paysToSelf
        ( mconcat
            [ singleton mockSgCs TestValues.newPriceTokenName 1
            , singleton mockSgCs secondTn 1
            , toValue minAdaTxOut
            ]
        )
        dtm
    ]
noMintCtx :: ContextBuilder ( 'ForSpending BuiltinData BuiltinData)
noMintCtx =
  mconcat
    [ mintsValue $
        mconcat
          [ singleton mockSgCs TestValues.newPriceTokenName 1
          , toValue minAdaTxOut
          , singleton mockSgCs secondTn (negate 1)
          , singleton mockSgCs (TokenName (unTokenName secondTn <> "x")) 1
          ]
    , paysToSelf
        ( mconcat
            [ singleton mockSgCs TestValues.newPriceTokenName 1
            , singleton mockSgCs (TokenName (unTokenName secondTn <> "x")) 1
            , singleton secondCs TestValues.tokenName 1
            , toValue minAdaTxOut
            ]
        )
        dtm
    ]
