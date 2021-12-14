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

module PabContracts.MutualBetContracts(
    MutualBetContracts(..)
    , handlers
    ) where

import           Control.Monad.Freer
import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics (Generic)
import           Prettyprinter
import           Data.Default                       (Default (def))
import           Language.PureScript.Bridge (argonaut, equal, genericShow, mkSumType)
import           Data.Row
import qualified Data.OpenApi.Schema as OpenApi
import           Playground.Types (FunctionSchema)
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (..), HasDefinitions (..), SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Simulator (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator as Simulator
import           Plutus.PAB.Run.PSGenerator (HasPSTypes (..))
import           Schema (FormSchema)
import           Contracts.MutualBet


data MutualBetContracts = 
    MutualBetOwner MutualBetStartParams
    | MutualBetUser MutualBetParams
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
    MutualBetOwner _    -> Builtin.endpointsToSchemas @Empty
    MutualBetUser _  -> Builtin.endpointsToSchemas @BettorSchema

getMutualBetContracts :: MutualBetContracts -> SomeBuiltin
getMutualBetContracts = \case
    MutualBetOwner params -> SomeBuiltin $ mutualBetStartWithOracle params
    MutualBetUser  params -> SomeBuiltin $ mutualBetBettor params

handlers :: SimulatorEffectHandlers (Builtin MutualBetContracts)
handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (contractHandler Builtin.handleBuiltin)