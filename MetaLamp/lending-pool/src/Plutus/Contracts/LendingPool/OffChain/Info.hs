{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Plutus.Contracts.LendingPool.OffChain.Info where

import qualified Control.Lens                                 as Lens
import           Control.Monad                                hiding (fmap)
import qualified Data.ByteString                              as BS
import qualified Data.Map                                     as Map
import           Data.Monoid                                  (Last (..))
import           Data.Proxy                                   (Proxy (..))
import           Data.Text                                    (Text, pack)
import qualified Data.Text                                    as Text
import           Data.Void                                    (Void)
import           Ext.Plutus.Ledger.Value                      (utxoValue)
import           Ledger                                       hiding (singleton)
import           Ledger.Constraints                           as Constraints
import           Ledger.Constraints.OnChain                   as Constraints
import           Ledger.Constraints.TxConstraints             as Constraints
import qualified Ledger.Scripts                               as Scripts
import qualified Ledger.Typed.Scripts                         as Scripts
import           Playground.Contract
import           Plutus.Abstract.ContractResponse             (ContractResponse,
                                                               withContractResponse)
import           Plutus.Abstract.OutputValue                  (OutputValue (..))
import qualified Plutus.Abstract.TxUtils                      as TxUtils
import           Plutus.Contract                              hiding (when)
import           Plutus.Contracts.Currency                    as Currency
import qualified Plutus.Contracts.LendingPool.OffChain.AToken as AToken
import qualified Plutus.Contracts.LendingPool.OffChain.State  as State
import           Plutus.Contracts.LendingPool.OnChain.Core    (Aave,
                                                               AaveDatum (..),
                                                               AaveRedeemer (..),
                                                               Reserve (..),
                                                               UserConfig (..))
import qualified Plutus.Contracts.LendingPool.OnChain.Core    as Core
import qualified Plutus.Contracts.Service.FungibleToken       as FungibleToken
import qualified Plutus.Contracts.Service.Oracle              as Oracle
import           Plutus.V1.Ledger.Ada                         (adaValueOf,
                                                               lovelaceValueOf)
import qualified Plutus.V1.Ledger.Address                     as Addr
import           Plutus.V1.Ledger.Value                       as Value
import qualified PlutusTx
import qualified PlutusTx.AssocMap                            as AssocMap
import           PlutusTx.Prelude                             hiding
                                                              (Monoid (..),
                                                               Semigroup (..),
                                                               mconcat, unless)
import           Prelude                                      (Monoid (..),
                                                               Semigroup (..),
                                                               show, subtract)
import qualified Prelude
import           Text.Printf                                  (printf)

-- | Gets current Lending Pool reserves state
reserves :: Aave -> Contract w s Text (AssocMap.Map AssetClass Reserve)
reserves aave = ovValue <$> State.findAaveReserves aave

-- | Gets current Lending Pool user configs state
users :: Aave -> Contract w s Text (AssocMap.Map (AssetClass, PubKeyHash) UserConfig)
users aave = ovValue <$> State.findAaveUserConfigs aave

fundsAt :: PubKeyHash -> Contract w s Text Value
fundsAt pkh = utxoValue <$> utxoAt (pubKeyHashAddress pkh)

-- | Gets all UTxOs belonging to the Lending Pool script and concats them into one Value
poolFunds :: Aave -> Contract w s Text Value
poolFunds aave = utxoValue <$> utxoAt (Core.aaveAddress aave)

type AaveInfoSchema =
    Endpoint "fundsAt" PubKeyHash
    .\/ Endpoint "poolFunds" ()
    .\/ Endpoint "reserves" ()
    .\/ Endpoint "users" ()

data InfoContractState =
    FundsAt Value
    | PoolFunds Value
    | Reserves (AssocMap.Map AssetClass Reserve)
    | Users (AssocMap.Map (AssetClass, PubKeyHash) UserConfig)
    deriving (Prelude.Eq, Show, Generic, FromJSON, ToJSON)

infoEndpoints :: Aave -> Contract (ContractResponse Text InfoContractState) AaveInfoSchema Void ()
infoEndpoints aave = forever $
    withContractResponse (Proxy @"fundsAt") FundsAt fundsAt
    `select` withContractResponse (Proxy @"poolFunds") PoolFunds (const $ poolFunds aave)
    `select` withContractResponse (Proxy @"reserves") Reserves (const $ reserves aave)
    `select` withContractResponse (Proxy @"users") Users (const $ users aave)
