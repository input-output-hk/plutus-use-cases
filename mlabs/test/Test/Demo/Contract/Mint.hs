{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Test.Demo.Contract.Mint
  ( test
  ) where

import Control.Lens
import Control.Monad hiding (fmap)
import qualified Data.Map as Map
import Ledger
import Ledger.Ada as Ada
import Ledger.Value
import Plutus.Contract.Test
import Plutus.Trace.Emulator as Emulator
import PlutusTx.Prelude
import Test.Tasty

import Mlabs.Demo.Contract.Mint

test :: TestTree
test = checkPredicateOptions
  (defaultCheckOptions & emulatorConfig .~ emCfg)
  "mint trace"
  (    walletFundsChange
      (Wallet 1)
      (Ada.lovelaceValueOf (-15_000_000) <> assetClassValue usdToken 15)
  .&&. walletFundsChange
         (Wallet 2)
         (  Ada.lovelaceValueOf (-50_000_000)
         <> assetClassValue usdToken 20
         <> assetClassValue cadToken 30
         )
  )
  mintTrace

emCfg :: EmulatorConfig
emCfg = EmulatorConfig $ Left $ Map.fromList [(Wallet 1, v), (Wallet 2, v)]
 where
  v :: Value
  v = Ada.lovelaceValueOf 100_000_000

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
  h1 <- activateContractWallet (Wallet 1) mintEndpoints
  h2 <- activateContractWallet (Wallet 2) mintEndpoints

  -- Scenario 1: Buy single currency.
  callEndpoint @"mint" h1 MintParams { mpTokenName = usd, mpAmount = 5 }
  void $ Emulator.waitNSlots 2
  callEndpoint @"mint" h1 MintParams { mpTokenName = usd, mpAmount = 10 }
  void $ Emulator.waitNSlots 2

  -- Scenario 2: Buy multiple currencies.
  callEndpoint @"mint" h2 MintParams { mpTokenName = usd, mpAmount = 20 }
  void $ Emulator.waitNSlots 2
  callEndpoint @"mint" h2 MintParams { mpTokenName = cad, mpAmount = 30 }
  void $ Emulator.waitNSlots 2





