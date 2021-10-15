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

module Plutus.Contracts.NftMarketplace.OnChain.Core.ID where

import           Control.Lens                                     ((&), (.~),
                                                                   (?~), (^.))
import qualified Control.Lens                                     as Lens
import qualified Crypto.Hash                                      as Hash
import qualified Data.Aeson                                       as J
import qualified Data.ByteArray                                   as BA
import qualified Data.List                                        as HL
import qualified Data.Text                                        as T
import qualified GHC.Generics                                     as Haskell
import           Ledger
import qualified Ledger.Constraints                               as Constraints
import qualified Ledger.Typed.Scripts                             as Scripts
import qualified Ledger.Value                                     as V
import           Plutus.Contract
import           Plutus.Contract.StateMachine
import           Plutus.Contracts.NftMarketplace.OnChain.Core.NFT
import qualified Plutus.Contracts.Services.Auction                as Auction
import qualified Plutus.Contracts.Services.Sale                   as Sale
import qualified PlutusTx
import qualified PlutusTx.AssocMap                                as AssocMap
import           PlutusTx.Prelude                                 hiding
                                                                  (Semigroup (..))
import           Prelude                                          (Semigroup (..))
import qualified Prelude                                          as Haskell

data InternalNftId = InternalNftId {
  iniIpfsCidHash :: !IpfsCidHash,
  iniIpfsCid     :: !IpfsCid
}
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON)

PlutusTx.unstableMakeIsData ''InternalNftId

PlutusTx.makeLift ''InternalNftId

data InternalBundleId = InternalBundleId {
  ibiBundleId :: !BundleId,
  ibiIpfsCids :: !(AssocMap.Map IpfsCidHash IpfsCid)
}
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON)

PlutusTx.unstableMakeIsData ''InternalBundleId

PlutusTx.makeLift ''InternalBundleId

data InternalId = 
  NftInternalId InternalNftId 
  | BundleInternalId InternalBundleId
  deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON)

PlutusTx.unstableMakeIsData ''InternalId

PlutusTx.makeLift ''InternalId