{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module PabContracts.SimulatorPabContracts(
    MutualBetContracts(..)
    , handlers
    ) where

import Contracts.MutualBet
import Contracts.Oracle
import Control.Monad.Freer
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (def))
import Data.OpenApi.Schema qualified as OpenApi
import Data.Row
import GHC.Generics (Generic)
import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (..), HasDefinitions (..), SomeBuiltin (..))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Simulator (SimulatorEffectHandlers)
import Plutus.PAB.Simulator qualified as Simulator
import Prettyprinter

data MutualBetContracts =
    MutualBetStartContract MutualBetStartParams
    | MutualBetBettorContract MutualBetParams
    | OracleСontract OracleParams
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty MutualBetContracts where
    pretty = viaShow

instance HasDefinitions MutualBetContracts where
    getDefinitions = []
    getSchema = \case
        MutualBetStartContract _  -> Builtin.endpointsToSchemas @Empty
        MutualBetBettorContract _ -> Builtin.endpointsToSchemas @BettorSchema
        OracleСontract _          -> Builtin.endpointsToSchemas @OracleSchema
    getContract = \case
        MutualBetStartContract params  -> SomeBuiltin $ mutualBetStartWithOracle params
        MutualBetBettorContract params -> SomeBuiltin $ mutualBetBettor params
        OracleСontract params          -> SomeBuiltin $ runOracle params

handlers :: SimulatorEffectHandlers (Builtin MutualBetContracts)
handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (contractHandler (Builtin.handleBuiltin @MutualBetContracts))
