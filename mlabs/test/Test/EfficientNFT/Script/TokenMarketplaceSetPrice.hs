module Test.EfficientNFT.Script.TokenMarketplaceSetPrice (test) where

import Prelude

import Ledger (CurrencySymbol, minAdaTxOut)
import Ledger.Value (CurrencySymbol (CurrencySymbol), TokenName (TokenName), singleton)
import Plutus.V1.Ledger.Ada (toValue)
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))
import PlutusTx.Builtins (BuiltinData)
import Test.Tasty (TestTree)
import Test.Tasty.Plutus.Context (ContextBuilder, Purpose (ForSpending), mintsValue, paysToSelf)
import Test.Tasty.Plutus.Script.Unit (shouldValidate, shouldn'tValidate)
import Test.Tasty.Plutus.TestData (TestData (SpendingTest))
import Test.Tasty.Plutus.WithScript (withTestScript)

import Mlabs.EfficientNFT.Types
import Test.EfficientNFT.Script.Values qualified as TestValues

test :: TestTree
test = withTestScript "Change price" TestValues.testMarketplaceScript $ do
  shouldValidate "Change price with valid data and context" validData validCtx

  shouldn'tValidate "Fail when consuming two inputs 1" twoInputsData1 twoInputsCtx1

  shouldn'tValidate "Fail when consuming two inputs 2" twoInputsData2 twoInputsCtx2

  shouldn'tValidate "Fail when not minting" validData noMintCtx

dtm :: BuiltinData
dtm = toBuiltinData ()

mockSgCs :: CurrencySymbol
mockSgCs = CurrencySymbol "ff"

secondCs :: CurrencySymbol
secondCs = CurrencySymbol "aa"

secondTn :: TokenName
secondTn = TokenName "foo"

validData :: TestData ( 'ForSpending BuiltinData MarketplaceAct)
validData = SpendingTest dtm redeemer val
  where
    redeemer = Update
    val =
      mconcat
        [ toValue minAdaTxOut
        , singleton mockSgCs TestValues.tokenName 1
        ]

twoInputsData1 :: TestData ( 'ForSpending BuiltinData MarketplaceAct)
twoInputsData1 = SpendingTest dtm redeemer val
  where
    redeemer = Update
    val =
      mconcat
        [ toValue minAdaTxOut
        , singleton mockSgCs TestValues.tokenName 1
        , singleton secondCs TestValues.tokenName 1
        ]

twoInputsData2 :: TestData ( 'ForSpending BuiltinData MarketplaceAct)
twoInputsData2 = SpendingTest dtm redeemer val
  where
    redeemer = Update
    val =
      mconcat
        [ toValue minAdaTxOut
        , singleton mockSgCs TestValues.tokenName 1
        , singleton mockSgCs secondTn 1
        ]

validCtx :: ContextBuilder ( 'ForSpending BuiltinData MarketplaceAct)
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

twoInputsCtx1 :: ContextBuilder ( 'ForSpending BuiltinData MarketplaceAct)
twoInputsCtx1 =
  mconcat
    [ mintsValue $
        mconcat
          [ singleton mockSgCs TestValues.tokenName (negate 1)
          , singleton mockSgCs TestValues.newPriceTokenName 1
          ]
    , paysToSelf
        ( mconcat
            [ singleton mockSgCs TestValues.newPriceTokenName 1
            , singleton secondCs TestValues.tokenName 1
            , toValue minAdaTxOut
            ]
        )
        dtm
    ]

twoInputsCtx2 :: ContextBuilder ( 'ForSpending BuiltinData MarketplaceAct)
twoInputsCtx2 =
  mconcat
    [ mintsValue $
        mconcat
          [ singleton mockSgCs TestValues.tokenName (negate 1)
          , singleton mockSgCs TestValues.newPriceTokenName 1
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

noMintCtx :: ContextBuilder ( 'ForSpending BuiltinData MarketplaceAct)
noMintCtx =
  mconcat
    [ paysToSelf
        ( mconcat
            [ singleton mockSgCs TestValues.tokenName 1
            , toValue minAdaTxOut
            ]
        )
        dtm
    ]
