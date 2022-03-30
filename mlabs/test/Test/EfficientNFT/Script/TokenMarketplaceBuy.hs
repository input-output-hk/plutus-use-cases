module Test.EfficientNFT.Script.TokenMarketplaceBuy (test) where

import Prelude

import Ledger (CurrencySymbol, minAdaTxOut, unPaymentPubKeyHash)
import Ledger.Value (CurrencySymbol (CurrencySymbol), TokenName (TokenName, unTokenName), assetClass, singleton)
import Plutus.V1.Ledger.Ada (toValue)
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))
import PlutusTx.Builtins (BuiltinData)
import Test.Tasty (TestTree)
import Test.Tasty.Plutus.Context (ContextBuilder, Purpose (ForSpending), mintsValue, paysToOther, paysToPubKeyWithDatum, paysToSelf)
import Test.Tasty.Plutus.Script.Unit (shouldValidate, shouldn'tValidate)
import Test.Tasty.Plutus.TestData (TestData (SpendingTest))
import Test.Tasty.Plutus.WithScript (withTestScript)

import Mlabs.EfficientNFT.Token (mkTokenName)
import Mlabs.EfficientNFT.Types (MarketplaceDatum (MarketplaceDatum))
import Test.EfficientNFT.Script.Values qualified as TestValues

test :: TestTree
test = withTestScript "Buy" TestValues.testMarketplaceScript $ do
  shouldValidate "Buy with valid data and context" validData validCtx

  shouldn'tValidate "Fail when not minting" validData noMintCtx

dtm :: MarketplaceDatum
dtm = MarketplaceDatum $ assetClass mockSgCs validOldTokenName

redeemer :: BuiltinData
redeemer = toBuiltinData ()

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

validData :: TestData ( 'ForSpending MarketplaceDatum BuiltinData)
validData = SpendingTest dtm redeemer val
  where
    val =
      mconcat
        [ toValue minAdaTxOut
        , singleton mockSgCs validOldTokenName 1
        ]

validCtx :: ContextBuilder ( 'ForSpending MarketplaceDatum BuiltinData)
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

noMintCtx :: ContextBuilder ( 'ForSpending MarketplaceDatum BuiltinData)
noMintCtx =
  mconcat
    [ paysToSelf
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
