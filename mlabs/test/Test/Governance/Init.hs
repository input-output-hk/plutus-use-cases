{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Init blockchain state for tests
module Test.Governance.Init (
  startGovernance,
  checkOptions,
  fstWalletWithGOV,
  sndWalletWithGOV,
  walletNoGOV,
  adminWallet,
  params,
  nft,
  gov,
  xgov,
  assertHasErrorOutcome,
  scriptAddress,
  startGovernance,
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

import Ledger (Address, CurrencySymbol, Value)
import Ledger qualified
import Plutus.Contract.Test (
  CheckOptions,
  Outcome (..),
  Wallet (..),
  assertOutcome,
  defaultCheckOptions,
  emulatorConfig,
  walletPubKey,
 )
import Plutus.Trace.Emulator (initialChainState)
import Plutus.V1.Ledger.Ada (adaSymbol, adaToken)
import Plutus.V1.Ledger.Value (TokenName (..))
import Plutus.V1.Ledger.Value qualified as Value (singleton)

import Test.Utils (next)

params :: Gov.GovParams
params = Gov.GovParams acNFT acGOV

acNFT :: Gov.AssetClassNft
acNFT = Gov.AssetClassNft "aa" "NFTToken"

acGOV :: Gov.AssetClassGov
acGOV = Gov.AssetClassGov "ff" "GOVToken"

startGovernance = Api.StartGovernance params

checkOptions :: CheckOptions
checkOptions = defaultCheckOptions & emulatorConfig . initialChainState .~ Left initialDistribution

-- | Wallets that are used for testing.
fstWalletWithGOV, sndWalletWithGOV, walletNoGOV, adminWallet :: Wallet
fstWalletWithGOV = Wallet 1
sndWalletWithGOV = Wallet 2
walletNoGOV = Wallet 3
adminWallet = Wallet 50

scriptAddress :: Address
scriptAddress = Gov.scrAddress params

-- | Make `GOV` `Value`
nft :: Integer -> Value
nft = Value.singleton cs tn
  where
    (Gov.AssetClassNft cs tn) = acNFT

-- | Make `GOV` `Value`
gov :: Integer -> Value
gov = Gov.govSingleton acGOV

-- | Make `xGOV` `Value`
xgov :: Wallet -> Integer -> Value
xgov wallet = Gov.xgovSingleton acNFT (mkTokenName wallet)
  where
    (Gov.AssetClassGov cs tn) = acGOV
    mkTokenName :: Wallet -> TokenName
    mkTokenName = TokenName . Ledger.getPubKeyHash . Ledger.pubKeyHash . walletPubKey

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
    , (adminWallet, ada 1000_000_000 <> nft 1)
    ]

-- | Assert that contract finished excution with arbitrary error
assertHasErrorOutcome contract tag =
  assertOutcome contract tag isFailed
  where
    isFailed e
      | (Failed _) <- e = True
      | otherwise = False
