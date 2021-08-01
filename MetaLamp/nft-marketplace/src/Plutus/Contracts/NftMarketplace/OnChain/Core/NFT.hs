{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fobject-code #-}

module Plutus.Contracts.NftMarketplace.OnChain.Core.NFT where

import           Control.Lens                   ((&), (.~), (?~), (^.))
import qualified Control.Lens                   as Lens
import qualified Data.Aeson                     as J
import qualified Data.Text                      as T
import qualified Ext.Plutus.Contracts.Auction   as Auction
import qualified GHC.Generics                   as Haskell
import           Ledger
import qualified Ledger.Constraints             as Constraints
import qualified Ledger.Typed.Scripts           as Scripts
import qualified Ledger.Value                   as V
import           Plutus.Contract
import           Plutus.Contract.StateMachine
import qualified Plutus.Contracts.Services.Sale as Sale
import qualified PlutusTx
import qualified PlutusTx.AssocMap              as AssocMap
import           PlutusTx.Prelude               hiding (Semigroup (..))
import           Prelude                        (Semigroup (..))
import qualified Prelude                        as Haskell

type IpfsCid = ByteString
type IpfsCidHash = ByteString
type Sale = (AssetClass, Sale.LovelacePrice, Value)
type Auction = (AssetClass, PubKeyHash, Value, Slot)

data Lot = Lot
                { lotLink    :: !(Either Sale Auction)
                , lotIpfsCid :: !ByteString
                }
  deriving stock (Haskell.Eq, Haskell.Show, Haskell.Generic)
  deriving anyclass (J.ToJSON, J.FromJSON)

PlutusTx.unstableMakeIsData ''Lot

PlutusTx.makeLift ''Lot

Lens.makeClassy_ ''Lot

data NFT =
  NFT
    { nftId          :: !CurrencySymbol
    , nftName        :: !ByteString
    , nftDescription :: !ByteString
    , nftIssuer      :: !(Maybe PubKeyHash)
    , nftLot         :: !(Maybe Lot)
    }
  deriving stock (Haskell.Eq, Haskell.Show, Haskell.Generic)
  deriving anyclass (J.ToJSON, J.FromJSON)

PlutusTx.unstableMakeIsData ''NFT

PlutusTx.makeLift ''NFT

Lens.makeClassy_ ''NFT
