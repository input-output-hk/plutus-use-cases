{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Marketplace.Fixtures.Script where

import           Ledger                                       (Address,
                                                               CurrencySymbol)
import qualified Ledger.Value                                 as V
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Marketplace

marketplace :: Marketplace.Marketplace
marketplace =
  Marketplace.Marketplace $
  V.assetClass marketplaceSymbol Marketplace.marketplaceProtocolName

marketplaceSymbol :: CurrencySymbol
marketplaceSymbol = "ff"

marketplaceAddress :: Address
marketplaceAddress = Marketplace.marketplaceAddress marketplace
