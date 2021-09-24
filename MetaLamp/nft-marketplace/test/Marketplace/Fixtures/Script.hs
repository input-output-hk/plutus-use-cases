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
import Plutus.Types.Percentage (Percentage(..))
import PlutusTx.Ratio 

marketplace :: Marketplace.Marketplace
marketplace =
  Marketplace.Marketplace {
    Marketplace.marketplaceOperator = pubKeyHash $ walletPubKey Fixtures.ownerWallet,
    Marketplace.marketplaceFee = percentage
  }
  
percentage :: Percentage
percentage = Percentage $ 5 % 2

marketplaceAddress :: Address
marketplaceAddress = Marketplace.marketplaceAddress marketplace
