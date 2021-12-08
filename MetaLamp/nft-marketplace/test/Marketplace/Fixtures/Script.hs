{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
module Marketplace.Fixtures.Script where

import           Ledger                                       (Address,
                                                               CurrencySymbol,
                                                               pubKeyHash)
import           Ledger.Ada                                   (Ada (..))
import qualified Ledger.Value                                 as V
import qualified Marketplace.Fixtures.Wallet                  as Fixtures
import           Plutus.Abstract.Percentage                   (Percentage (..))
import           Plutus.Abstract.PercentageInterface          (calculatePercentageRounded)
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Marketplace
import           Wallet.Emulator.Types                        (Wallet (..),
                                                               walletPubKeyHash)
import Plutus.Contracts.NftMarketplace.OffChain.Serialization (deserializeByteString)

marketplace :: Marketplace.Marketplace
marketplace =
  Marketplace.Marketplace {
    Marketplace.marketplaceOperator = walletPubKeyHash Fixtures.ownerWallet,
    Marketplace.marketplaceName = deserializeByteString "Metalamp nft marketplace",
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
