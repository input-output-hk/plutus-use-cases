module Test.EfficientNFT.Script.TokenMarketplaceRedeem (test) where

import Prelude

import Ledger (CurrencySymbol, minAdaTxOut, unPaymentPubKeyHash)
import Ledger.Value (CurrencySymbol (CurrencySymbol), TokenName (TokenName), assetClass, singleton, unTokenName)
import Plutus.V1.Ledger.Ada (toValue)
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))
import PlutusTx.Builtins (BuiltinData)
import Test.Tasty (TestTree)
import Test.Tasty.Plutus.Context (ContextBuilder, Purpose (ForSpending), mintsValue, paysToPubKey, signedWith)
import Test.Tasty.Plutus.Script.Unit (shouldValidate, shouldn'tValidate)
import Test.Tasty.Plutus.TestData (TestData (SpendingTest))
import Test.Tasty.Plutus.WithScript (withTestScript)

import Mlabs.EfficientNFT.Types (MarketplaceDatum (MarketplaceDatum))
import Test.EfficientNFT.Script.Values qualified as TestValues

test :: TestTree
test = withTestScript "Redeem" TestValues.testMarketplaceScript $ do
  shouldValidate "Redeem with valid data and context" validData validCtx

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
    , paysToPubKey
        (unPaymentPubKeyHash TestValues.authorPkh)
        ( mconcat
            [ singleton mockSgCs TestValues.newPriceTokenName 1
            , toValue minAdaTxOut
            ]
        )
    , signedWith (unPaymentPubKeyHash TestValues.authorPkh)
    ]

noMintCtx :: ContextBuilder ( 'ForSpending MarketplaceDatum BuiltinData)
noMintCtx =
  mconcat
    [ mintsValue $
        mconcat
          [ singleton mockSgCs TestValues.newPriceTokenName 1
          , toValue minAdaTxOut
          , singleton mockSgCs secondTn (negate 1)
          , singleton mockSgCs (TokenName (unTokenName secondTn <> "x")) 1
          ]
    , paysToPubKey
        (unPaymentPubKeyHash TestValues.authorPkh)
        ( mconcat
            [ singleton mockSgCs TestValues.newPriceTokenName 1
            , singleton mockSgCs (TokenName (unTokenName secondTn <> "x")) 1
            , singleton secondCs TestValues.tokenName 1
            , toValue minAdaTxOut
            ]
        )
    ]
