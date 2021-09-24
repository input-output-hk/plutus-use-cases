{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Plutus.Contracts.NftMarketplace.OnChain.Core.Marketplace where
import qualified Data.Aeson              as J
import           GHC.Generics            (Generic)
import           Ledger
import           Plutus.Types.Percentage (Percentage)
import qualified PlutusTx
import           Prelude

data Marketplace =
  Marketplace
    { marketplaceOperator :: PubKeyHash,
      marketplaceNFTFee   :: Value,  -- fixed fee by minting and bundling
      marketplaceSaleFee  :: Percentage -- percentage by selling on the Sale or Auction
    }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (J.ToJSON, J.FromJSON)

PlutusTx.makeLift ''Marketplace
PlutusTx.unstableMakeIsData ''Marketplace
