{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Init blockchain state for tests
module Test.Governance.Init (
    startGovernance
  , checkOptions
  , fstWalletWithGOV
  , sndWalletWithGOV
  , walletNoGOV
  , adminWallet
  , params
  , nft
  , gov
  , xgov
  , assertHasErrorOutcome
  , scriptAddress
  , startGovernance
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
import Plutus.V1.Ledger.Value (TokenName(..))
import qualified Plutus.V1.Ledger.Value as Value (singleton)

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
walletNoGOV      = Wallet 3
adminWallet      = Wallet 50

scriptAddress :: Address
scriptAddress = Gov.scrAddress params

-- | Make `GOV` `Value`
nft :: Integer -> Value
nft = Value.singleton cs tn
  where (Gov.AssetClassNft cs tn) = acNFT

-- | Make `GOV` `Value`
gov :: Integer -> Value
gov = Gov.govSingleton acGOV

-- | Make `xGOV` `Value`
xgov :: Wallet -> Integer -> Value
xgov wallet value = Gov.xgovSingleton 
  acNFT
  (mkTokenName wallet)
  value
  where 
    (Gov.AssetClassGov cs tn) = acGOV
    mkTokenName :: Wallet -> TokenName
    mkTokenName = TokenName . Ledger.getPubKeyHash . Ledger.pubKeyHash . walletPubKey

-- | Make `Ada` `Value`
ada :: Integer -> Value
ada x = Value.singleton adaSymbol adaToken x

-- | wallets for tests
initialDistribution :: M.Map Wallet Value
initialDistribution = M.fromList
  [ (fstWalletWithGOV, ada 1000_000_000 <> gov 100)
  , (sndWalletWithGOV, ada 1000_000_000 <> gov 100)
  , (walletNoGOV,      ada 1000_000_000)
  , (adminWallet,      ada 1000_000_000 <> nft 10)
  ]
    
-- | Assert that contract finished excution with arbitrary error
assertHasErrorOutcome contract tag message = 
  assertOutcome contract tag isFailed message
  where
    isFailed e
      | (Failed _) <- e = True
      | otherwise       = False
