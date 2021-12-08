{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
module Marketplace.Spec.Start
  ( tests, startTrace
  ) where

import           Control.Monad                                  (void)
import           Data.Text                                      (Text)
import           Ledger.Ada                                     (Ada (..),
                                                                 toValue)
import           Ledger.Index                                   (minAdaTxOut)
import qualified Ledger.Value                                   as V
import qualified Marketplace.Fixtures                           as Fixtures
import           Plutus.Abstract.Percentage                     (getPercentage)
import           Plutus.Contract                                (Contract)
import           Plutus.Contract.Test
import qualified Plutus.Contracts.NftMarketplace.Endpoints      as Marketplace
import           Plutus.Contracts.NftMarketplace.OffChain.Owner (StartMarketplaceParams (..))
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core   as Marketplace
import qualified Plutus.Trace                                   as Trace
import qualified PlutusTx.AssocMap                              as AssocMap
import           Test.Tasty

tests :: TestTree
tests =
  testGroup
    "start"
    [ checkPredicateOptions
        Fixtures.options
        "Should start a new marketplace with empty store"
        (datumsCheck .&&. marketplaceOpenedWithMinAdaTxOut .&&. marketplaceOperatorPayedMinAdaTxOut)
        startTrace
    ]

startTrace :: Trace.EmulatorTrace ()
startTrace = do
  h <- Trace.activateContractWallet Fixtures.ownerWallet Marketplace.ownerEndpoints
  _ <- Trace.callEndpoint @"start" h startMarketplaceParams
  _ <- Trace.waitNSlots 50
  pure ()

startMarketplaceParams :: StartMarketplaceParams
startMarketplaceParams = StartMarketplaceParams {
    marketplaceName = "Metalamp nft marketplace",
    creationFee = getLovelace $ Fixtures.marketplaceCreationFee,
    saleFee = getPercentage Fixtures.percentage
}

startContract ::
     Contract () Marketplace.MarketplaceOwnerSchema Text Marketplace.Marketplace
startContract = Marketplace.start startMarketplaceParams

datumsCheck :: TracePredicate
datumsCheck =
  dataAtAddress
    Fixtures.marketplaceAddress
    (== [Marketplace.MarketplaceDatum AssocMap.empty])

marketplaceOpenedWithMinAdaTxOut :: TracePredicate
marketplaceOpenedWithMinAdaTxOut =
  valueAtAddress Fixtures.marketplaceAddress (toValue minAdaTxOut ==)

marketplaceOperatorPayedMinAdaTxOut :: TracePredicate
marketplaceOperatorPayedMinAdaTxOut =
  walletFundsChange Fixtures.ownerWallet $ toValue (- minAdaTxOut)
