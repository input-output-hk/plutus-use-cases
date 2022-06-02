module Test.EfficientNFT.Script.TokenMarketplaceSetPrice (test) where

import Prelude

import Ledger (CurrencySymbol, minAdaTxOut)
import Ledger.Value (CurrencySymbol (CurrencySymbol), TokenName (TokenName), assetClass, singleton, unTokenName)
import Plutus.V1.Ledger.Ada (toValue)
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))
import PlutusTx.Builtins (BuiltinData)
import Test.Tasty (TestTree)
import Test.Tasty.Plutus.Context (ContextBuilder, Purpose (ForSpending), mintsValue, paysToSelf)
import Test.Tasty.Plutus.Script.Unit (shouldValidate, shouldn'tValidate)
import Test.Tasty.Plutus.TestData (TestData (SpendingTest))
import Test.Tasty.Plutus.WithScript (withTestScript)

import Mlabs.EfficientNFT.Types (MarketplaceDatum (MarketplaceDatum))
import Test.EfficientNFT.Script.Values qualified as TestValues

test :: TestTree
test = withTestScript "Change price" TestValues.testMarketplaceScript $ do
  shouldValidate "Change price with valid data and context" validData validCtx

  shouldn'tValidate "Fail when not minting" validData noMintCtx

dtm :: MarketplaceDatum
dtm = MarketplaceDatum $ assetClass mockSgCs TestValues.tokenName

redeemer :: BuiltinData
redeemer = toBuiltinData ()

mockSgCs :: CurrencySymbol
mockSgCs = CurrencySymbol "ff"

secondCs :: CurrencySymbol
secondCs = CurrencySymbol "aa"

secondTn :: TokenName
secondTn = TokenName "foo"

validData :: TestData ( 'ForSpending MarketplaceDatum BuiltinData)
validData = SpendingTest dtm redeemer val
  where
    val =
      mconcat
        [ toValue minAdaTxOut
        , singleton mockSgCs TestValues.tokenName 1
        ]

validCtx :: ContextBuilder ( 'ForSpending MarketplaceDatum BuiltinData)
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

noMintCtx :: ContextBuilder ( 'ForSpending MarketplaceDatum BuiltinData)
noMintCtx =
  mconcat
    [ paysToSelf
        ( mconcat
            [ singleton mockSgCs TestValues.newPriceTokenName 1
            , singleton mockSgCs (TokenName (unTokenName secondTn <> "x")) 1
            , singleton secondCs TestValues.tokenName 1
            , toValue minAdaTxOut
            ]
        )
        dtm
    ]
