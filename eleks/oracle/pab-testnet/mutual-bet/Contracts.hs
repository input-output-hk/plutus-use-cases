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

module Contracts(
    OracleContracts(..)
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
import           Data.Text (Text)
import           Ledger (TxId)
import           Playground.Types (FunctionSchema)
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (..), HasDefinitions (..), SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Simulator (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator as Simulator
import           Plutus.PAB.Run.PSGenerator (HasPSTypes (..))
import           Schema (FormSchema)
import           Contracts.MutualBet
import           Plutus.Contract (Contract)
import           Types.Game (GameId)

data MutualBetContracts = 
    MutualBetOwner OracleParams
    | MutualBetUser Oracle
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty MutualBetContracts where
    pretty = viaShow

instance HasPSTypes MutualBetContracts where
    psTypes =
        [ equal . genericShow . argonaut $ mkSumType @MutualBetContracts
        ]
instance HasDefinitions MutualBetContracts where
    getDefinitions = [
                     ]
    getContract = getMutualBetContracts
    getSchema = getMutualBetContractsSchema

getMutualBetContractsSchema :: MutualBetContracts -> [FunctionSchema FormSchema]
getMutualBetContractsSchema = \case
    MutualBetOwner _    -> Builtin.endpointsToSchemas @OracleSchema
    MutualBetUser _ -> Builtin.endpointsToSchemas @Empty

getMutualBetContracts :: MutualBetContracts -> SomeBuiltin
getMutualBetContracts = \case
    MutualBetOwner params -> SomeBuiltin $ runOracle params
    MutualBetUser oracle -> SomeBuiltin $ (requestOracleForAddress oracle 1 :: Contract () Empty Text ())

handlers :: SimulatorEffectHandlers (Builtin MutualBetContracts)
handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (contractHandler Builtin.handleBuiltin)
