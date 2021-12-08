{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Plutus.Contracts.NftMarketplace.OnChain.Core.Marketplace where
import qualified Data.Aeson                 as J
import qualified Data.OpenApi.Schema        as OpenApi
import           GHC.Generics               (Generic)
import           Ledger
import           Plutus.Abstract.Percentage (Percentage)
import qualified PlutusTx
import           Prelude
import           PlutusTx.Builtins.Internal                             (BuiltinByteString)

data Marketplace =
  Marketplace
    { marketplaceOperator :: PubKeyHash,
      marketplaceName     :: BuiltinByteString,
      marketplaceNFTFee   :: Ada,  -- fixed fee by minting and bundling
      marketplaceSaleFee  :: Percentage -- percentage by selling on the Sale
    }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (J.ToJSON, J.FromJSON, OpenApi.ToSchema)

PlutusTx.makeLift ''Marketplace
PlutusTx.unstableMakeIsData ''Marketplace
