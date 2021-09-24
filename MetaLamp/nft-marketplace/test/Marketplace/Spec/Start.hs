{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Marketplace.Spec.Start
  ( tests, startTrace
  ) where

import           Control.Monad                                  (void)
import           Data.Text                                      (Text)
import qualified Ledger.Value                                   as V
import qualified Marketplace.Fixtures                           as Fixtures
import           Plutus.Contract                                (Contract)
import           Plutus.Contract.Test
import qualified Plutus.Contracts.NftMarketplace.Endpoints      as Marketplace
import           Plutus.Contracts.NftMarketplace.OffChain.Owner (StartMarketplaceParams (..))
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core   as Marketplace
import qualified Plutus.Trace                                   as Trace
import           Plutus.Types.Percentage                        (getPercentage)
import qualified PlutusTx.AssocMap                              as AssocMap
import           Test.Tasty

tests :: TestTree
tests =
  testGroup
    "start"
    [ checkPredicateOptions
        Fixtures.options
        "Should start a new marketplace with empty store"
        datumsCheck
        startTrace
    ]

startTrace :: Trace.EmulatorTrace ()
startTrace = do
  _ <- Trace.activateContractWallet Fixtures.ownerWallet $ void startContract
  _ <- Trace.waitNSlots 50
  pure ()

startMarketplaceParams :: StartMarketplaceParams
startMarketplaceParams = StartMarketplaceParams {
    nftFee = 100000,  -- 0.1 ADA
    saleFee =getPercentage Fixtures.percentage
}

startContract ::
     Contract () Marketplace.MarketplaceOwnerSchema Text Marketplace.Marketplace
startContract = Marketplace.start startMarketplaceParams

datumsCheck :: TracePredicate
datumsCheck =
  dataAtAddress
    Fixtures.marketplaceAddress
    (== [Marketplace.MarketplaceDatum AssocMap.empty AssocMap.empty])
