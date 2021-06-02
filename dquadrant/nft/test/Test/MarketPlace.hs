{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
-- | Implements a custom currency with a monetary policy that allows
--   the forging of a fixed amount of units.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Test.MarketPlace
where

import Control.Monad ( Monad((>>), (>>=)), void )
import           GHC.Generics              (Generic)
import           Plutus.Contract        as Contract hiding (when)
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema, adaCurrency)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Plutus.Contract.Blockchain.MarketPlace
import           Wallet.Emulator.Wallet
import Data.Void
import Data.Aeson
import qualified Prelude


defaultMarket ::Market
defaultMarket=Market (pubKeyHash ( walletPubKey (Wallet 10))) 2


test :: Prelude.IO ()
test= runEmulatorTraceIO test1

defaultAsset=assetClass  (CurrencySymbol "abcd") (TokenName "")

test1::EmulatorTrace ()
test1=do
        h1 <- activateContractWallet (Wallet 1) (endpoints  defaultMarket)
        h2 <- activateContractWallet (Wallet 2) (endpoints  defaultMarket)
        void $ Emulator.waitNSlots 1
-- I am not sure what's the error here, I am not familiar with the effect system.
        void $ callEndpoint  h2 @"sell" (SellParams defaultAsset 32)
