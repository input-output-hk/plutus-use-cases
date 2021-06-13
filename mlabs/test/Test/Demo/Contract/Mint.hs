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
import Control.Monad.Freer.Extras as Extras
import Data.Default (Default(..))
import qualified Data.Map as Map
import Data.Monoid (Last(..))
import Ledger
import Ledger.Ada as Ada
import Ledger.Value
import Plutus.Contract.Test
import Plutus.Trace.Emulator as Emulator
import PlutusTx.Prelude
import Prelude (IO, Show(..), String)
import Test.Tasty

import Mlabs.Demo.Contract.Mint

test :: TestTree
test = checkPredicateOptions
  (defaultCheckOptions & emulatorConfig .~ emCfg)
  "mint trace"
  (    walletFundsChange
      (Wallet 1)
      (Ada.lovelaceValueOf (-10_000_000) <> assetClassValue token 1000)
  .&&. walletFundsChange
         (Wallet 2)
         (Ada.lovelaceValueOf (-50_000_000) <> assetClassValue token 50)
  .&&. walletFundsChange
         (Wallet 3)
         (Ada.lovelaceValueOf 0 <> assetClassValue token 0)
  )
  myTrace

runMyTrace :: IO ()
runMyTrace = runEmulatorTraceIO' def emCfg myTrace

emCfg :: EmulatorConfig
emCfg = EmulatorConfig $ Left $ Map.fromList
  [ (Wallet w, v) | w <- [1 .. 3] ]
 where
  v :: Value
  v = Ada.lovelaceValueOf 100_000_000

usd :: TokenName
usd = "USD"

curSymbol :: CurrencySymbol
curSymbol = getCurrencySymbol usd 0

token :: AssetClass
token = AssetClass (curSymbol, usd)

myTrace :: EmulatorTrace ()
myTrace = do
  h1 <- activateContractWallet (Wallet 1) mintEndpoints
  h2 <- activateContractWallet (Wallet 2) mintEndpoints
  h3 <- activateContractWallet (Wallet 3) mintEndpoints

  callEndpoint @"mint" h1 MintParams { mpTokenName = usd, mpAmount = 10 }
  void $ Emulator.waitNSlots 2

  callEndpoint @"mint" h2 MintParams { mpTokenName = usd, mpAmount = 25 }
  void $ Emulator.waitNSlots 2
  callEndpoint @"mint" h2 MintParams { mpTokenName = usd, mpAmount = 25 }
  void $ Emulator.waitNSlots 2

  callEndpoint @"mint" h3 MintParams { mpTokenName = usd, mpAmount = 0 }
  void $ Emulator.waitNSlots 2


