{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Plutus.Contracts.NftMarketplace.OnChain.Core.StateMachine where

import qualified Data.Aeson        as J
import qualified Data.Text         as T
import qualified GHC.Generics      as Haskell
import           Ledger
import           Plutus.Contract
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import           PlutusTx.Prelude  hiding (Semigroup (..))
import           Prelude           (Semigroup (..))
import qualified Prelude           as Haskell

newtype Marketplace =
  Marketplace
    { marketplaceProtocolToken :: AssetClass
    }
  deriving  (Haskell.Eq, Haskell.Ord, Haskell.Show, Haskell.Generic)
  deriving anyclass (J.ToJSON, J.FromJSON)

PlutusTx.makeLift ''Marketplace

type IpfsCidHash = ByteString

data NFT =
  NFT
    { nftId          :: CurrencySymbol
    , nftName        :: ByteString
    , nftDescription :: ByteString
    , nftIssuer      :: Maybe PubKeyHash
    }
  deriving  (Haskell.Eq, Haskell.Show, Haskell.Generic)
  deriving anyclass (J.ToJSON, J.FromJSON)

PlutusTx.unstableMakeIsData ''NFT

PlutusTx.makeLift ''NFT

data MarketplaceRedeemer
  = StartRedeemer
  | AddNftRedeemer IpfsCidHash NFT
  deriving  (Haskell.Show)

PlutusTx.unstableMakeIsData ''MarketplaceRedeemer

PlutusTx.makeLift ''MarketplaceRedeemer

data MarketplaceDatum =
  MarketplaceDatum
    { getMarketplaceDatum :: AssocMap.Map IpfsCidHash NFT
    }
  deriving  (Haskell.Show)

PlutusTx.unstableMakeIsData ''MarketplaceDatum

PlutusTx.makeLift ''MarketplaceDatum
