{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Plutus.Contracts.NftMarketplace.OnChain.Core
  ( module Export
  , module Plutus.Contracts.NftMarketplace.OnChain.Core
  ) where

import qualified Data.Aeson                                                as J
import qualified Data.Text                                                 as T
import qualified GHC.Generics                                              as Haskell
import           Ledger
import qualified Ledger.Typed.Scripts                                      as Scripts
import           Ledger.Value
import           Plutus.Contract
import           Plutus.Contract.StateMachine
import           Plutus.Contracts.NftMarketplace.OnChain.Core.ID           as Export
import           Plutus.Contracts.NftMarketplace.OnChain.Core.NFT          as Export
import           Plutus.Contracts.NftMarketplace.OnChain.Core.StateMachine as Export
import qualified PlutusTx
import qualified PlutusTx.AssocMap                                         as AssocMap
import           PlutusTx.Prelude                                          hiding
                                                                           (Semigroup (..))
import           Prelude                                                   (Semigroup (..))
import qualified Prelude                                                   as Haskell
import Plutus.Types.Marketplace as Export

marketplaceValidator :: Marketplace -> Validator
marketplaceValidator = Scripts.validatorScript . marketplaceInst

marketplaceAddress :: Marketplace -> Ledger.Address
marketplaceAddress = scriptAddress . marketplaceValidator
