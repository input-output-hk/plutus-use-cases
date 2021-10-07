{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Marketplace.Fixtures.Script where

import           Ledger                                       (Address,
                                                               CurrencySymbol,
                                                               pubKeyHash)
import           Ledger.Ada                                   (lovelaceValueOf)
import qualified Ledger.Value                                 as V
import qualified Marketplace.Fixtures.Wallet                  as Fixtures
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Marketplace
import           Plutus.Abstract.Percentage                      (Percentage (..))
import           Wallet.Emulator.Types                        (Wallet (..),
                                                               walletPubKey)

marketplace :: Marketplace.Marketplace
marketplace =
  Marketplace.Marketplace {
    Marketplace.marketplaceOperator = pubKeyHash $ walletPubKey Fixtures.ownerWallet,
    Marketplace.marketplaceSaleFee = percentage,
    Marketplace.marketplaceNFTFee = lovelaceValueOf 100000  -- 0.1 ADA
  }

percentage :: Percentage
percentage = Percentage (5, 2)

marketplaceAddress :: Address
marketplaceAddress = Marketplace.marketplaceAddress marketplace
