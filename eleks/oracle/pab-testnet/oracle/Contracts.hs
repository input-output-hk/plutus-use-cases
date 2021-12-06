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
import           Contracts.Oracle
import           Plutus.Contract (Contract)
import           Types.Game (GameId)

data OracleContracts = 
    OracleContract OracleParams
    | OracleRequest Oracle
    | OracleUse Oracle
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty OracleContracts where
    pretty = viaShow

instance HasPSTypes OracleContracts where
    psTypes =
        [ equal . genericShow . argonaut $ mkSumType @OracleContracts
        ]
instance HasDefinitions OracleContracts where
    getDefinitions = [
                     ]
    getContract = getOracleContracts
    getSchema = getOracleContractsSchema

getOracleContractsSchema :: OracleContracts -> [FunctionSchema FormSchema]
getOracleContractsSchema = \case
    OracleContract _    -> Builtin.endpointsToSchemas @OracleSchema
    OracleRequest _ -> Builtin.endpointsToSchemas @Empty
    OracleUse _ -> Builtin.endpointsToSchemas @UseOracleSchema

getOracleContracts :: OracleContracts -> SomeBuiltin
getOracleContracts = \case
    OracleContract params -> SomeBuiltin $ runOracle params
    OracleRequest oracle -> SomeBuiltin $ (requestOracleForAddress oracle 1 :: Contract () Empty Text ())
    OracleUse oracle -> SomeBuiltin $ (useOracle oracle)

handlers :: SimulatorEffectHandlers (Builtin OracleContracts)
handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (contractHandler Builtin.handleBuiltin)
