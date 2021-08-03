{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Init blockchain state for tests
module Test.Governance.Init where

import Prelude ()
import PlutusTx.Prelude

import Control.Lens ((&), (.~))
import Data.Map (Map)
import qualified Data.Map as M

import qualified Mlabs.Governance.Contract.Validation as Gov
import qualified Mlabs.Governance.Contract.Server     as Gov

import Plutus.Contract.Test (
  CheckOptions, defaultCheckOptions, emulatorConfig
  , Wallet(..), walletPubKey, assertOutcome, Outcome(..))
import Plutus.Trace.Emulator ( initialChainState)
import Ledger (Address, Value, CurrencySymbol)
import qualified Ledger
import Plutus.V1.Ledger.Ada (adaSymbol, adaToken)
-- import Plutus.V1.Ledger.Value (Value)
import qualified Plutus.V1.Ledger.Value as Value (singleton)

import Test.Utils (next)


checkOptions :: CheckOptions
checkOptions = defaultCheckOptions & emulatorConfig . initialChainState .~ Left initialDistribution

-- | Wallets that are used for testing.
fstWalletWithGOV, sndWalletWithGOV, walletNoGOV :: Wallet
fstWalletWithGOV = Wallet 1
sndWalletWithGOV = Wallet 2
walletNoGOV      = Wallet 3

testGovCurrencySymbol :: CurrencySymbol
testGovCurrencySymbol = "ff"

scriptAddress :: Address
scriptAddress = Gov.scrAddress testGovCurrencySymbol

-- | Make `GOV` `Value`
gov :: Integer -> Value
gov = Gov.govValueOf testGovCurrencySymbol
-- | Make `GOV` `Value`

xgov :: Integer -> Value
xgov = Value.singleton 
  (Ledger.scriptCurrencySymbol $ Gov.xGovMintingPolicy testGovCurrencySymbol)
  (Gov.xgovToken)

-- | Make `Ada` `Value`
ada :: Integer -> Value
ada x = Value.singleton adaSymbol adaToken x

-- | wallets for tests
initialDistribution :: M.Map Wallet Value
initialDistribution = M.fromList
  [ (fstWalletWithGOV,  ada 1000_000_000 <> gov 100)
  , (sndWalletWithGOV, ada 1000_000_000 <> gov 100)
  , (walletNoGOV,      ada 1000_000_000)
  ]
    
-- | Assert that contract finished excution with arbitrary error
assertHasErrorOutcome contract tag message = 
  assertOutcome contract tag isFailed message
  where
    isFailed e
      | (Failed _) <- e = True
      | otherwise       = False