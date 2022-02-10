module Test.EfficientNFT.Script.TokenMarketplaceBuy (test) where

import Prelude

import Ledger (CurrencySymbol, minAdaTxOut, unPaymentPubKeyHash)
import Ledger.Value (CurrencySymbol (CurrencySymbol), TokenName (TokenName), singleton)
import Plutus.V1.Ledger.Ada (toValue)
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))
import PlutusTx.Builtins (BuiltinData)
import Test.Tasty (TestTree)
import Test.Tasty.Plutus.Context (ContextBuilder, Purpose (ForSpending), mintsValue, paysToOther, paysToPubKeyWithDatum, paysToSelf)
import Test.Tasty.Plutus.Script.Unit (shouldValidate, shouldn'tValidate)
import Test.Tasty.Plutus.TestData (TestData (SpendingTest))
import Test.Tasty.Plutus.WithScript (withTestScript)

import Mlabs.EfficientNFT.Token (mkTokenName)
import Mlabs.EfficientNFT.Types
import Test.EfficientNFT.Script.Values qualified as TestValues

test :: TestTree
test = withTestScript "Buy" TestValues.testMarketplaceScript $ do
  shouldValidate "Buy with valid data and context" validData validCtx

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

validOldTokenName :: TokenName
validOldTokenName = mkTokenName TestValues.nft2

validNewTokenName :: TokenName
validNewTokenName = mkTokenName TestValues.nft3

validData :: TestData ( 'ForSpending BuiltinData MarketplaceAct)
validData = SpendingTest dtm redeemer val
  where
    redeemer = Update
    val =
      mconcat
        [ toValue minAdaTxOut
        , singleton mockSgCs validOldTokenName 1
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

noMintCtx :: ContextBuilder ( 'ForSpending BuiltinData MarketplaceAct)
noMintCtx =
  mconcat
    [ paysToSelf
        ( mconcat
            [ singleton mockSgCs validOldTokenName 1
            , toValue minAdaTxOut
            ]
        )
        dtm
    , royalitiesCtx
    ]

twoInputsCtx1 :: ContextBuilder ( 'ForSpending BuiltinData MarketplaceAct)
twoInputsCtx1 =
  mconcat
    [ mintsValue $
        mconcat
          [ singleton mockSgCs validOldTokenName (negate 1)
          , singleton mockSgCs validNewTokenName 1
          ]
    , paysToSelf
        ( mconcat
            [ singleton mockSgCs validNewTokenName 1
            , singleton secondCs validNewTokenName 1
            , toValue minAdaTxOut
            ]
        )
        dtm
    , royalitiesCtx
    ]

twoInputsCtx2 :: ContextBuilder ( 'ForSpending BuiltinData MarketplaceAct)
twoInputsCtx2 =
  mconcat
    [ mintsValue $
        mconcat
          [ singleton mockSgCs validOldTokenName (negate 1)
          , singleton mockSgCs validNewTokenName 1
          ]
    , paysToSelf
        ( mconcat
            [ singleton mockSgCs validNewTokenName 1
            , singleton mockSgCs secondTn 1
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
