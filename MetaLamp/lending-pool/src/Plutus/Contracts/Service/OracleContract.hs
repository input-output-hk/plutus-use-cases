{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Plutus.Contracts.Service.OracleContract where

import qualified Plutus.Contracts.Service.Oracle as Oracle
import           Data.Monoid               (Last (..))
import           Plutus.Contract           as Contract
import           Ledger.Value                         (AssetClass(..), assetClass, CurrencySymbol, TokenName)
import qualified Control.Lens                                 as Lens
import           Data.Aeson                (FromJSON, ToJSON)
import           GHC.Generics              (Generic)
import           Data.Text                 (Text, pack)
import           Schema                    (ToSchema)
import qualified PlutusTx
import           Plutus.Abstract.ContractResponse             (ContractResponse, withContractResponse)
import Control.Monad (forever)
import           Data.Void                                    (Void)
import Data.Proxy

data StartOracleParams = 
    StartOracleParams {
        assetSymbol :: CurrencySymbol, 
        assetToken :: TokenName,
        fees :: Integer}
    deriving stock (Prelude.Eq, Prelude.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''StartOracleParams
PlutusTx.makeLift ''StartOracleParams

start :: forall w s. HasBlockchainActions s => StartOracleParams -> Contract w s Text Oracle.Oracle
start StartOracleParams {..} = do
    let oracleParams = Oracle.OracleParams
            { opFees   = fees
            , opSymbol = assetSymbol
            , opToken  = assetToken
            }
    oracle <- Oracle.startOracle oracleParams
    pure oracle

data UpdateOracleParams = 
    UpdateOracleParams {
        oracle :: Oracle.Oracle,
        rate :: Integer -- TODO: not sure about field name
    }
    deriving stock (Prelude.Eq, Prelude.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''UpdateOracleParams
PlutusTx.makeLift ''UpdateOracleParams

update :: forall w s. HasBlockchainActions s => UpdateOracleParams -> Contract w s Text Oracle.Oracle
update UpdateOracleParams {..} = do 
    Oracle.updateOracle oracle rate
    pure oracle

type OracleSchema = 
    BlockchainActions 
    .\/ Endpoint "start" StartOracleParams
    .\/ Endpoint "update" UpdateOracleParams

data OracleContractState =
    Started
    | Updated 
    deriving (Prelude.Eq, Show, Generic, FromJSON, ToJSON)

oracleEndpoints :: Contract (ContractResponse Text OracleContractState) OracleSchema Void ()
oracleEndpoints = forever $
    withContractResponse (Proxy @"start") (const Started) start
    `select` withContractResponse (Proxy @"update") (const Updated) update
