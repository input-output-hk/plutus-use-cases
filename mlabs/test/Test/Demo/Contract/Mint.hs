{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.Demo.Contract.Mint (
  test,
) where

import PlutusTx.Prelude

import Control.Lens ((&), (.~))
import Control.Monad (void)
import Data.Default (def)
import Data.Map qualified as Map
import Ledger.Ada (lovelaceValueOf)
import Ledger.Value (AssetClass (..), TokenName, Value, assetClassValue)
import Plutus.Contract.Test qualified as Test
import Plutus.Trace.Emulator as Emulator
import Test.Tasty (TestTree)

import Mlabs.Demo.Contract.Mint (MintParams (..), curSymbol, mintEndpoints)

test :: TestTree
test =
  Test.checkPredicateOptions
    (Test.defaultCheckOptions & Test.emulatorConfig .~ emCfg)
    "mint trace"
    ( Test.walletFundsChange
        Test.w1
        (lovelaceValueOf (-15_000_000) <> assetClassValue usdToken 15)
        Test..&&. Test.walletFundsChange
          Test.w2
          ( lovelaceValueOf (-50_000_000)
              <> assetClassValue usdToken 20
              <> assetClassValue cadToken 30
          )
    )
    mintTrace

emCfg :: EmulatorConfig
emCfg =
  EmulatorConfig
    (Left $ Map.fromList [(Test.w1, v), (Test.w2, v)])
    def
    def
  where
    v :: Value
    v = lovelaceValueOf 100_000_000

usd :: TokenName
usd = "USD"

cad :: TokenName
cad = "CAD"

usdToken :: AssetClass
usdToken = AssetClass (curSymbol, usd)

cadToken :: AssetClass
cadToken = AssetClass (curSymbol, cad)

mintTrace :: EmulatorTrace ()
mintTrace = do
  h1 <- activateContractWallet Test.w1 mintEndpoints
  h2 <- activateContractWallet Test.w2 mintEndpoints

  -- Scenario 1: Buy single currency.
  callEndpoint @"mint" h1 MintParams {mpTokenName = usd, mpAmount = 5}
  void $ Emulator.waitNSlots 2
  callEndpoint @"mint" h1 MintParams {mpTokenName = usd, mpAmount = 10}
  void $ Emulator.waitNSlots 2

  -- Scenario 2: Buy multiple currencies.
  callEndpoint @"mint" h2 MintParams {mpTokenName = usd, mpAmount = 20}
  void $ Emulator.waitNSlots 2
  callEndpoint @"mint" h2 MintParams {mpTokenName = cad, mpAmount = 30}
  void $ Emulator.waitNSlots 2
