{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Init blockchain state for tests
module Test.Governance.Init (
  checkOptions,
  fstWalletWithGOV,
  sndWalletWithGOV,
  walletNoGOV,
  adminWallet,
  gov,
  acGOV,
  xgov,
  xgovEP,
  assertHasErrorOutcome,
  scriptAddress,
) where

import PlutusTx.Prelude
import Prelude ()

import Control.Lens ((&), (.~))
import Data.Coerce (coerce)
import Data.Map (Map)
import Data.Map qualified as M

import Mlabs.Governance.Contract.Api qualified as Api
import Mlabs.Governance.Contract.Server qualified as Gov
import Mlabs.Governance.Contract.Validation qualified as Gov

import Ledger (Address, CurrencySymbol, Value, unPaymentPubKeyHash)
import Ledger qualified
import Mlabs.Utils.Wallet (walletFromNumber)

import Plutus.Contract.Test (
  CheckOptions,
  Outcome (..),
  Wallet (..),
  assertOutcome,
  defaultCheckOptions,
  emulatorConfig,
  mockWalletPaymentPubKeyHash,
 )

import Plutus.Trace.Emulator (initialChainState)
import Plutus.V1.Ledger.Ada (adaSymbol, adaToken)
import Plutus.V1.Ledger.Value qualified as Value (singleton)

import Test.Utils (next)

acGOV :: Gov.AssetClassGov
acGOV = Gov.AssetClassGov "ff" "GOVToken"

checkOptions :: CheckOptions
checkOptions = defaultCheckOptions & emulatorConfig . initialChainState .~ Left initialDistribution

-- | Wallets that are used for testing.
fstWalletWithGOV, sndWalletWithGOV, walletNoGOV, adminWallet :: Wallet
fstWalletWithGOV = walletFromNumber 1
sndWalletWithGOV = walletFromNumber 2
walletNoGOV = walletFromNumber 3
adminWallet = walletFromNumber 4

scriptAddress :: Address
scriptAddress = Gov.govAddress acGOV

-- | Make `GOV` `Value`
gov :: Integer -> Value
gov = Gov.govSingleton acGOV

-- | Make `xGOV` `Value`
xgov :: Wallet -> Integer -> Value
xgov wallet =
  Gov.xgovSingleton
    acGOV
    (mkPkh wallet)
  where
    (Gov.AssetClassGov cs tn) = acGOV
    mkPkh :: Wallet -> Ledger.PubKeyHash
    mkPkh = unPaymentPubKeyHash . mockWalletPaymentPubKeyHash

xgovEP :: Wallet -> Integer -> [(Ledger.PubKeyHash, Integer)]
xgovEP wallet value = [(mkPkh wallet, value)]
  where
    (Gov.AssetClassGov cs tn) = acGOV
    mkPkh :: Wallet -> Ledger.PubKeyHash
    mkPkh = unPaymentPubKeyHash . mockWalletPaymentPubKeyHash

-- | Make `Ada` `Value`
ada :: Integer -> Value
ada = Value.singleton adaSymbol adaToken

-- | wallets for tests
initialDistribution :: M.Map Wallet Value
initialDistribution =
  M.fromList
    [ (fstWalletWithGOV, ada 1000_000_000 <> gov 100)
    , (sndWalletWithGOV, ada 1000_000_000 <> gov 100)
    , (walletNoGOV, ada 1000_000_000)
    , (adminWallet, ada 1000_000_000)
    ]

-- | Assert that contract finished excution with arbitrary error
assertHasErrorOutcome contract tag =
  assertOutcome contract tag isFailed
  where
    isFailed e
      | (Failed _) <- e = True
      | otherwise = False
