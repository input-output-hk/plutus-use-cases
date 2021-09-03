{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Marketplace.Fixtures.Script where

import           Ledger                                       (Address,pubKeyHash,
                                                               CurrencySymbol)
import qualified Ledger.Value                                 as V
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Marketplace
import qualified Marketplace.Fixtures.Wallet as Fixtures
import           Wallet.Emulator.Types                        (Wallet (..),
                                                               walletPubKey)
                                                               
marketplace :: Marketplace.Marketplace
marketplace =
  Marketplace.Marketplace $
  pubKeyHash $ walletPubKey Fixtures.ownerWallet

marketplaceAddress :: Address
marketplaceAddress = Marketplace.marketplaceAddress marketplace
