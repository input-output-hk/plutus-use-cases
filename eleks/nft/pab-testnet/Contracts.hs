
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE NumericUnderscores  #-}

module Contracts(
    NFTMarketContracts(..)
    , handlers
    ) where

import           Control.Monad.Freer
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Default (Default (def))
import           GHC.Generics (Generic)
import           Prettyprinter

import           Language.PureScript.Bridge (argonaut, equal, genericShow, mkSumType, order)
import           Language.PureScript.Bridge.TypeParameters (A)
import           Data.Row
import qualified Data.OpenApi.Schema as OpenApi
import           Ledger (TxId)
import           Playground.Types (FunctionSchema)
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (..), HasDefinitions (..), SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Simulator (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator as Simulator
import           Plutus.PAB.Run.PSGenerator (HasPSTypes (..))
import           Schema (FormSchema)
import           Contracts.NFT as NFTMarket

data NFTMarketContracts =
    NFTStartContract Integer
    | NFTUserContract NFTMarket.NFTMarket
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty NFTMarketContracts where
    pretty = viaShow

instance HasPSTypes NFTMarketContracts where
    psTypes =
        [ equal . genericShow . argonaut $ mkSumType @NFTMarketContracts
        -- These types come from the Uniswap contract and need to be available in PS
        -- , equal . genericShow . argonaut $ mkSumType @Uniswap
        -- , equal . genericShow . argonaut $ mkSumType @(Coin A)
        -- , order . equal . genericShow $ argonaut $ mkSumType @U
        ]

instance HasDefinitions NFTMarketContracts where
    getDefinitions = [
                     ]
    getContract = getNFTContracts
    getSchema = getNFTContractsSchema

getNFTContractsSchema :: NFTMarketContracts -> [FunctionSchema FormSchema]
getNFTContractsSchema = \case
    NFTStartContract _ -> Builtin.endpointsToSchemas @NFTMarket.MarketOwnerSchema
    NFTUserContract _ -> Builtin.endpointsToSchemas @NFTMarket.MarketUserSchema

getNFTContracts :: NFTMarketContracts -> SomeBuiltin
getNFTContracts = \case
    NFTStartContract fee -> SomeBuiltin (NFTMarket.ownerEndpoint NFTMarket.forgeMarketToken fee)
    NFTUserContract market -> SomeBuiltin (NFTMarket.userEndpoints market)

handlers :: SimulatorEffectHandlers (Builtin NFTMarketContracts)
handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (contractHandler Builtin.handleBuiltin)