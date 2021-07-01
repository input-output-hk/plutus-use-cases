{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fobject-code #-}

module Plutus.Contracts.LendingPool.OnChain.Core (module Plutus.Contracts.LendingPool.OnChain.Core, module Export, Aave(..), aaveInstance) where

import           Control.Lens                                        ((^?))
import qualified Control.Lens                                        as Lens
import           Control.Monad                                       hiding
                                                                     (fmap)
import qualified Data.ByteString                                     as BS
import qualified Data.Map                                            as Map
import           Data.Text                                           (Text,
                                                                      pack)
import           Data.Void                                           (Void)
import           Ext.Plutus.Ledger.Contexts                          (findOnlyOneDatumHashByValue,
                                                                      findValueByDatumHash,
                                                                      parseDatum,
                                                                      scriptInputsAt,
                                                                      valueSpentFrom)
import           Ledger                                              hiding
                                                                     (singleton)
import           Ledger.Constraints                                  as Constraints
import           Ledger.Constraints.OnChain                          as Constraints
import           Ledger.Constraints.TxConstraints                    as Constraints
import qualified Ledger.Scripts                                      as UntypedScripts
import qualified Ledger.Typed.Scripts                                as Scripts
import           Playground.Contract
import           Plutus.Contract                                     hiding
                                                                     (when)
import           Plutus.Contracts.LendingPool.OnChain.Core.Script    (AaveDatum,
                                                                      AaveRedeemer,
                                                                      AaveScript)
import           Plutus.Contracts.LendingPool.OnChain.Core.Script    as Export
import           Plutus.Contracts.LendingPool.OnChain.Core.Validator (Aave (..),
                                                                      aaveInstance)
import qualified Plutus.Contracts.Service.Oracle                     as Oracle
import           Plutus.V1.Ledger.Value
import qualified PlutusTx
import qualified PlutusTx.AssocMap                                   as AssocMap
import           PlutusTx.Prelude                                    hiding
                                                                     (Semigroup (..),
                                                                      unless)
import           Prelude                                             (Semigroup (..))
import qualified Prelude

aaveProtocolName :: TokenName
aaveProtocolName = "Aave"

aaveValidator :: Aave -> Validator
aaveValidator = Scripts.validatorScript . aaveInstance

aaveHash :: Aave -> Ledger.ValidatorHash
aaveHash = UntypedScripts.validatorHash . aaveValidator

aaveAddress :: Aave -> Ledger.Address
aaveAddress = Ledger.scriptAddress . aaveValidator

aave :: CurrencySymbol -> Aave
aave protocol = Aave (assetClass protocol aaveProtocolName)
