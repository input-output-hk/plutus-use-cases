{-# LANGUAGE OverloadedStrings #-}
module Ext.Value where

import           Plutus.Abstract.Percentage
import           Prelude
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Data.Map               as Map
import qualified Ledger.Value                                             as V
import Ext.Plutus.Ledger.Value (isNftInValue)

currencySymbol :: V.CurrencySymbol
currencySymbol =  V.CurrencySymbol "50b69b375c08e6b43a5deca05e9f10214d86f9f84745594a26b4725e"

tokenName :: V.TokenName
tokenName = V.TokenName "QmPeoJnaDttpFrSySYBY3reRFCzL3qv4Uiqz376EBv9W16"

value :: V.Value
value = V.singleton currencySymbol tokenName 1

valueWithAnotherToken :: V.Value
valueWithAnotherToken = V.singleton currencySymbol (V.TokenName "anotherTokenName") 1

valueWithAnotherCurrency :: V.Value
valueWithAnotherCurrency =  V.singleton (V.CurrencySymbol "anotherCurrency") tokenName 1

valueWithoutNFT :: V.Value
valueWithoutNFT =  V.singleton currencySymbol tokenName 11

emptyValue :: V.Value
emptyValue = mempty

tests :: TestTree
tests = testGroup "isNftInValue"
  [ testCase "should be in value when currency with token are in the value" $
    (isNftInValue value currencySymbol tokenName) @?= True,
    testCase "should be not in value when token isn't in the value" $
    (isNftInValue valueWithAnotherToken currencySymbol tokenName) @?= False,
    testCase "should be not in value if currency isn't in value" $
    (isNftInValue valueWithAnotherCurrency currencySymbol tokenName) @?= False,
    testCase "should be not in value if more than 1 token in value" $
    (isNftInValue valueWithoutNFT currencySymbol tokenName) @?= False,
    testCase "should be not in value if value is empty" $
    (isNftInValue emptyValue currencySymbol tokenName) @?= False
  ]
