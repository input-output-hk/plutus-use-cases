{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Marketplace.Spec.Start
  ( tests, startTrace
  ) where

import           Control.Monad                                (void)
import           Data.Text                                    (Text)
import qualified Ledger.Value                                 as V
import qualified Marketplace.Fixtures                         as Fixtures
import           Plutus.Contract                              (Contract)
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
        Fixtures.options
        "Should start a new marketplace with empty store"
        (datumsCheck .&&. valueCheck)
        startTrace
    ]

startTrace :: Trace.EmulatorTrace ()
startTrace = do
  _ <- Trace.activateContractWallet Fixtures.ownerWallet $ void startContract
  _ <- Trace.waitNSlots 50
  pure ()

startContract ::
     Contract () Marketplace.MarketplaceOwnerSchema Text Marketplace.Marketplace
startContract = Marketplace.start' $ pure Fixtures.marketplaceSymbol

datumsCheck :: TracePredicate
datumsCheck =
  dataAtAddress
    Fixtures.marketplaceAddress
    (== Marketplace.MarketplaceDatum AssocMap.empty AssocMap.empty)

valueCheck :: TracePredicate
valueCheck =
  valueAtAddress
    Fixtures.marketplaceAddress
    (== V.singleton Fixtures.marketplaceSymbol Marketplace.marketplaceProtocolName 1)
