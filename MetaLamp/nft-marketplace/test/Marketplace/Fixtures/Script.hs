{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module Marketplace.Fixtures.Script where

import           Control.Lens                                 ((&), (.~))
import qualified Control.Lens                                 as Lens
import           Control.Monad                                (void)
import qualified Data.Map                                     as Map
import           Data.Text                                    (Text)
import           Ledger
import qualified Ledger.Ada                                   as Ada
import qualified Ledger.Value                                 as V
import           Plutus.Contract
import           Plutus.Contract.Test
import qualified Plutus.Contracts.NftMarketplace.Endpoints    as Marketplace
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Marketplace
import qualified Plutus.Trace                                 as Trace
import qualified PlutusTx.AssocMap                            as AssocMap
import           Test.Tasty

marketplace :: Marketplace.Marketplace
marketplace =
  Marketplace.Marketplace $
  V.assetClass marketplaceSymbol Marketplace.marketplaceProtocolName

marketplaceSymbol :: CurrencySymbol
marketplaceSymbol = "ff"

marketplaceAddress :: Address
marketplaceAddress = Marketplace.marketplaceAddress marketplace
