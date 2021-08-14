{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module Marketplace.Spec.Start
  ( tests
  ) where

import           Control.Lens                                 ((&), (.~))
import qualified Control.Lens                                 as Lens
import           Control.Monad                                (void)
import qualified Data.Map                                     as Map
import           Data.Text                                    (Text)
import           Ledger
import qualified Ledger.Ada                                   as Ada
import qualified Ledger.Value                                 as V
import qualified Marketplace.Fixtures                         as Fixtures
import           Plutus.Contract
import           Plutus.Contract.Test
import qualified Plutus.Contracts.NftMarketplace.Endpoints    as Marketplace
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Marketplace
import qualified Plutus.Trace                                 as Trace
import qualified PlutusTx.AssocMap                            as AssocMap
import           Test.Tasty

tests :: TestTree
tests =
  testGroup
    "start"
    [ checkPredicateOptions
        options
        "Should start a new marketplace with empty store"
        datumsCheck
        startTrace
    ]

startTrace :: Trace.EmulatorTrace ()
startTrace = do
  _ <- Trace.activateContractWallet Fixtures.ownerWallet $ void startContract
  _ <- Trace.waitNSlots 5
  pure ()

startContract ::
     Contract () Marketplace.MarketplaceOwnerSchema Text Marketplace.Marketplace
startContract = Marketplace.start' $ pure marketplaceSymbol

datumsCheck :: TracePredicate
datumsCheck =
  dataAtAddress
    (Marketplace.marketplaceAddress marketplace)
    (== Marketplace.MarketplaceDatum AssocMap.empty AssocMap.empty)

options :: CheckOptions
options = defaultCheckOptions & emulatorConfig .~ emulatorCfg
  where
    emulatorCfg :: Trace.EmulatorConfig
    emulatorCfg =
      Trace.EmulatorConfig $ Left $ Map.singleton Fixtures.ownerWallet v
    v :: Value
    v =
      Ada.lovelaceValueOf 1000 _000_000 <>
      V.singleton marketplaceSymbol Marketplace.marketplaceProtocolName 1

marketplace :: Marketplace.Marketplace
marketplace =
  Marketplace.Marketplace $
  V.assetClass marketplaceSymbol Marketplace.marketplaceProtocolName

marketplaceSymbol :: CurrencySymbol
marketplaceSymbol = "ff"
