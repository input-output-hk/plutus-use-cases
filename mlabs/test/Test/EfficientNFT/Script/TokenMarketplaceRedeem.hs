module Test.EfficientNFT.Script.TokenMarketplaceRedeem (test) where

import Prelude

import Ledger (CurrencySymbol, minAdaTxOut, unPaymentPubKeyHash)
import Ledger.Value (CurrencySymbol (CurrencySymbol), TokenName (TokenName), singleton)
import Plutus.V1.Ledger.Ada (toValue)
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))
import PlutusTx.Builtins (BuiltinData)
import Test.Tasty (TestTree)
import Test.Tasty.Plutus.Context (ContextBuilder, Purpose (ForSpending), paysToPubKey, signedWith)
import Test.Tasty.Plutus.Script.Unit (shouldValidate, shouldn'tValidate)
import Test.Tasty.Plutus.TestData (TestData (SpendingTest))
import Test.Tasty.Plutus.WithScript (withTestScript)

import Mlabs.EfficientNFT.Types
import Test.EfficientNFT.Script.Values qualified as TestValues

test :: TestTree
test = withTestScript "Redeem" TestValues.testMarketplaceScript $ do
  shouldValidate "Redeem with valid data and context" validData validCtx

  shouldn'tValidate "Fail when consuming two inputs 1" twoInputsData1 twoInputsCtx1

  shouldn'tValidate "Fail when consuming two inputs 2" twoInputsData2 twoInputsCtx2

  shouldn'tValidate "Fail when not signed by the owner" validData noSignCtx

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
    redeemer = Redeem TestValues.nft1
    val =
      mconcat
        [ toValue minAdaTxOut
        , singleton mockSgCs TestValues.tokenName 1
        ]

twoInputsData1 :: TestData ( 'ForSpending BuiltinData MarketplaceAct)
twoInputsData1 = SpendingTest dtm redeemer val
  where
    redeemer = Redeem TestValues.nft1
    val =
      mconcat
        [ toValue minAdaTxOut
        , singleton mockSgCs TestValues.tokenName 1
        , singleton secondCs TestValues.tokenName 1
        ]

twoInputsData2 :: TestData ( 'ForSpending BuiltinData MarketplaceAct)
twoInputsData2 = SpendingTest dtm redeemer val
  where
    redeemer = Redeem TestValues.nft1
    val =
      mconcat
        [ toValue minAdaTxOut
        , singleton mockSgCs TestValues.tokenName 1
        , singleton mockSgCs secondTn 1
        ]

validCtx :: ContextBuilder ( 'ForSpending BuiltinData MarketplaceAct)
validCtx =
  mconcat
    [ paysToPubKey
        (unPaymentPubKeyHash TestValues.authorPkh)
        ( mconcat
            [ singleton mockSgCs TestValues.tokenName 1
            , toValue minAdaTxOut
            ]
        )
    , signedWith (unPaymentPubKeyHash TestValues.authorPkh)
    ]

twoInputsCtx1 :: ContextBuilder ( 'ForSpending BuiltinData MarketplaceAct)
twoInputsCtx1 =
  mconcat
    [ paysToPubKey
        (unPaymentPubKeyHash TestValues.authorPkh)
        ( mconcat
            [ singleton mockSgCs TestValues.tokenName 1
            , singleton secondCs TestValues.tokenName 1
            , toValue minAdaTxOut
            ]
        )
    , signedWith (unPaymentPubKeyHash TestValues.authorPkh)
    ]

twoInputsCtx2 :: ContextBuilder ( 'ForSpending BuiltinData MarketplaceAct)
twoInputsCtx2 =
  mconcat
    [ paysToPubKey
        (unPaymentPubKeyHash TestValues.authorPkh)
        ( mconcat
            [ singleton mockSgCs TestValues.tokenName 1
            , singleton mockSgCs secondTn 1
            , toValue minAdaTxOut
            ]
        )
    , signedWith (unPaymentPubKeyHash TestValues.authorPkh)
    ]

noSignCtx :: ContextBuilder ( 'ForSpending BuiltinData MarketplaceAct)
noSignCtx =
  mconcat
    [ paysToPubKey
        (unPaymentPubKeyHash TestValues.authorPkh)
        ( mconcat
            [ singleton mockSgCs TestValues.tokenName 1
            , toValue minAdaTxOut
            ]
        )
    ]
