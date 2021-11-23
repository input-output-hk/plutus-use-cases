{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
module Marketplace.Fixtures.Script where

import           Ledger                                       (Address,
                                                               CurrencySymbol,
                                                               pubKeyHash)
import           Ledger.Ada                                   (Ada (..))
import qualified Ledger.Value                                 as V
import qualified Marketplace.Fixtures.Wallet                  as Fixtures
import           Plutus.Abstract.Percentage                   (Percentage (..))
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Marketplace
import           Wallet.Emulator.Types                        (Wallet (..),
                                                               walletPubKeyHash)
import Plutus.Abstract.PercentageInterface (calculatePercentageRounded)

marketplace :: Marketplace.Marketplace
marketplace =
  Marketplace.Marketplace {
    Marketplace.marketplaceOperator = walletPubKeyHash Fixtures.ownerWallet,
    Marketplace.marketplaceSaleFee = percentage,
    Marketplace.marketplaceNFTFee = marketplaceCreationFee
  }

marketplaceCreationFee :: Ada
marketplaceCreationFee = Lovelace 2_100_000 -- 2.1 ADA (should be gte then minAdaTxOut)

percentage :: Percentage
percentage = Percentage (7, 2)

roundedPercentage :: Integer -> Integer
roundedPercentage price = calculatePercentageRounded percentage price

marketplaceAddress :: Address
marketplaceAddress = Marketplace.marketplaceAddress marketplace
