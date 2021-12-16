{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
module Plutus.TestnetMVP.OnChain.Script where

import qualified Control.Lens                                     as Lens
import qualified Data.Aeson                 as J
import qualified Data.OpenApi.Schema        as OpenApi
import           GHC.Generics               (Generic)
import           Ledger
import qualified Ledger.Typed.Scripts             as Scripts
import           Plutus.Abstract.Percentage (Percentage)
import qualified PlutusTx
import           Prelude
import           PlutusTx.Builtins.Internal  (BuiltinByteString)  
import Plutus.TestnetMVP.OnChain.ID (InternalNftId(..), InternalId(..))
import Plutus.TestnetMVP.OnChain.NFT (LotLink, NFT, IpfsCidHash, NftInfo(..))
import qualified Prelude                                          as Haskell
import qualified GHC.Generics                                     as Haskell
import qualified PlutusTx.AssocMap                                      as AssocMap

data Marketplace =
    Marketplace
        { marketplaceOperator :: PaymentPubKeyHash,
        marketplaceThreadToken :: AssetClass,
        marketplaceName     :: BuiltinByteString,
        marketplaceNFTFee   :: Ada,  -- fixed fee by minting and bundling
        marketplaceSaleFee  :: Percentage -- percentage by selling on the Sale
        }
    deriving stock (Eq, Show, Ord, Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, OpenApi.ToSchema)

PlutusTx.makeLift ''Marketplace
PlutusTx.unstableMakeIsData ''Marketplace

data PutOnSaleRedeemerValue =
  PutNftLotRedeemer InternalNftId LotLink
  deriving  (Haskell.Show)

PlutusTx.unstableMakeIsData ''PutOnSaleRedeemerValue

PlutusTx.makeLift ''PutOnSaleRedeemerValue

data RemoveFromSaleRedeemerValue =
  RemoveNftLotRedeemer IpfsCidHash
  deriving  (Haskell.Show)

PlutusTx.unstableMakeIsData ''RemoveFromSaleRedeemerValue

PlutusTx.makeLift ''RemoveFromSaleRedeemerValue

data MarketplaceRedeemer
  = CreateNftRedeemer IpfsCidHash NftInfo
  | PutOnSaleRedeemer PutOnSaleRedeemerValue
  | RemoveFromSaleRedeemer RemoveFromSaleRedeemerValue
  deriving  (Haskell.Show)

PlutusTx.unstableMakeIsData ''MarketplaceRedeemer

PlutusTx.makeLift ''MarketplaceRedeemer

mkPutOnSaleRedeemer :: InternalId -> LotLink -> MarketplaceRedeemer
mkPutOnSaleRedeemer (NftInternalId nId) lot = PutOnSaleRedeemer $ PutNftLotRedeemer nId lot

mkRemoveFromSaleRedeemer :: InternalId -> MarketplaceRedeemer
mkRemoveFromSaleRedeemer (NftInternalId nId) = RemoveFromSaleRedeemer . RemoveNftLotRedeemer $ iniIpfsCidHash  nId

data MarketplaceDatum =
  MarketplaceDatum
    {
      mdSingletons :: AssocMap.Map IpfsCidHash NFT
    }
  deriving stock (Haskell.Eq, Haskell.Show, Haskell.Generic)
  deriving anyclass (J.ToJSON, J.FromJSON)

PlutusTx.unstableMakeIsData ''MarketplaceDatum

PlutusTx.makeLift ''MarketplaceDatum

Lens.makeClassy_ ''MarketplaceDatum

data MarketplaceScript
instance Scripts.ValidatorTypes MarketplaceScript where
    type instance DatumType MarketplaceScript = MarketplaceDatum
    type instance RedeemerType MarketplaceScript = MarketplaceRedeemer
