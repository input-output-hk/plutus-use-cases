{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Init blockchain state for tests
module Test.Governance.Init (
    checkOptions
  , fstWalletWithGOV
  , sndWalletWithGOV
  , walletNoGOV
  , adminWallet
  , gov
  , acGOV
  , xgov
  , xgovEP
  , assertHasErrorOutcome
  , scriptAddress
) where

import Prelude ()
import PlutusTx.Prelude

import Control.Lens ((&), (.~))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Coerce (coerce)

import qualified Mlabs.Governance.Contract.Validation as Gov
import qualified Mlabs.Governance.Contract.Server     as Gov
import qualified Mlabs.Governance.Contract.Api        as Api

import Plutus.Contract.Test (
  CheckOptions, defaultCheckOptions, emulatorConfig
  , Wallet(..), walletPubKey, assertOutcome, Outcome(..))
import Plutus.Trace.Emulator ( initialChainState)
import Ledger (Address, Value, CurrencySymbol)
import qualified Ledger
import Plutus.V1.Ledger.Ada (adaSymbol, adaToken)
import qualified Plutus.V1.Ledger.Value as Value (singleton)

import Test.Utils (next)

acGOV :: Gov.AssetClassGov 
acGOV = Gov.AssetClassGov "ff" "GOVToken"

checkOptions :: CheckOptions
checkOptions = defaultCheckOptions & emulatorConfig . initialChainState .~ Left initialDistribution

-- | Wallets that are used for testing.
fstWalletWithGOV, sndWalletWithGOV, walletNoGOV, adminWallet :: Wallet
fstWalletWithGOV = Wallet 1
sndWalletWithGOV = Wallet 2
walletNoGOV      = Wallet 3
adminWallet      = Wallet 50

scriptAddress :: Address
scriptAddress = Gov.govAddress acGOV

-- | Make `GOV` `Value`
gov :: Integer -> Value
gov = Gov.govSingleton acGOV

-- | Make `xGOV` `Value`
xgov :: Wallet -> Integer -> Value
xgov wallet value = Gov.xgovSingleton 
  acGOV
  (mkPkh wallet)
  value
  where 
    (Gov.AssetClassGov cs tn) = acGOV
    mkPkh :: Wallet -> Ledger.PubKeyHash
    mkPkh = Ledger.pubKeyHash . walletPubKey

xgovEP :: Wallet -> Integer -> [(Ledger.PubKeyHash, Integer)]
xgovEP wallet value = [(mkPkh wallet, value)]
  where 
    (Gov.AssetClassGov cs tn) = acGOV
    mkPkh :: Wallet -> Ledger.PubKeyHash
    mkPkh = Ledger.pubKeyHash . walletPubKey

-- | Make `Ada` `Value`
ada :: Integer -> Value
ada x = Value.singleton adaSymbol adaToken x

-- | wallets for tests
initialDistribution :: M.Map Wallet Value
initialDistribution = M.fromList
  [ (fstWalletWithGOV, ada 1000_000_000 <> gov 100)
  , (sndWalletWithGOV, ada 1000_000_000 <> gov 100)
  , (walletNoGOV,      ada 1000_000_000)
  , (adminWallet,      ada 1000_000_000)
  ]
    
-- | Assert that contract finished excution with arbitrary error
assertHasErrorOutcome contract tag message = 
  assertOutcome contract tag isFailed message
  where
    isFailed e
      | (Failed _) <- e = True
      | otherwise       = False
