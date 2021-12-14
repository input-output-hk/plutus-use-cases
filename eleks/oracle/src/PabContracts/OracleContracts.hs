{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module PabContracts.OracleContracts(
    OracleContracts(..)
    , handlers
    ) where

import Control.Monad.Freer
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (def))
import GHC.Generics (Generic)
import Prettyprinter

import Contracts.Oracle
import Data.OpenApi.Schema qualified as OpenApi
import Data.Row
import Data.Text (Text)
import Language.PureScript.Bridge (argonaut, equal, genericShow, mkSumType)
import Playground.Types (FunctionSchema)
import Plutus.Contract (Contract)
import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (..), HasDefinitions (..), SomeBuiltin (..))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Run.PSGenerator (HasPSTypes (..))
import Plutus.PAB.Simulator (SimulatorEffectHandlers)
import Plutus.PAB.Simulator qualified as Simulator
import Schema (FormSchema)

data OracleContracts =
    OracleContract OracleParams
    | OracleRequest Oracle
    | OracleRedeemRequest Oracle
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
    OracleContract _      -> Builtin.endpointsToSchemas @OracleSchema
    OracleRequest _       -> Builtin.endpointsToSchemas @Empty
    OracleRedeemRequest _ -> Builtin.endpointsToSchemas @RedeemOracleSchema

getOracleContracts :: OracleContracts -> SomeBuiltin
getOracleContracts = \case
    OracleContract params      -> SomeBuiltin $ runOracle params
    OracleRequest oracle       -> SomeBuiltin $ (requestOracleForAddress oracle 1 :: Contract () Empty Text ())
    OracleRedeemRequest oracle -> SomeBuiltin $ (redeemOracle oracle)

handlers :: SimulatorEffectHandlers (Builtin OracleContracts)
handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (contractHandler Builtin.handleBuiltin)
