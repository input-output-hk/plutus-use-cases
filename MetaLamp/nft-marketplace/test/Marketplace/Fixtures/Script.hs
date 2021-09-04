{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Marketplace.Fixtures.Script where

import           Ledger                                       (Address,
                                                               CurrencySymbol,
                                                               pubKeyHash)
import qualified Ledger.Value                                 as V
import qualified Marketplace.Fixtures.Wallet                  as Fixtures
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Marketplace
import           Wallet.Emulator.Types                        (Wallet (..),
                                                               walletPubKey)

marketplace :: Marketplace.Marketplace
marketplace =
  Marketplace.Marketplace $
  pubKeyHash $ walletPubKey Fixtures.ownerWallet

marketplaceAddress :: Address
marketplaceAddress = Marketplace.marketplaceAddress marketplace
