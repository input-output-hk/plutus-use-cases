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

module PabContracts.SimulatorPabContractsSM(
    MutualBetContracts(..)
    , handlers
    ) where

import Contracts.MutualBetSM
import Contracts.Oracle
import Control.Monad.Freer
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (def))
import Data.OpenApi.Schema qualified as OpenApi
import GHC.Generics (Generic)
import Ledger.TimeSlot (SlotConfig)
import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (..), HasDefinitions (..), SomeBuiltin (..))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Simulator (SimulatorEffectHandlers)
import Plutus.PAB.Simulator qualified as Simulator
import Prettyprinter

data MutualBetContracts =
    MutualBetStartContract MutualBetParams
    | MutualBetBettorContract SlotConfig ThreadToken MutualBetParams
    | OracleСontract OracleParams
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty MutualBetContracts where
    pretty = viaShow

instance HasDefinitions MutualBetContracts where
    getDefinitions = []
    getSchema = \case
        MutualBetStartContract _      -> Builtin.endpointsToSchemas @MutualBetStartSchema
        MutualBetBettorContract _ _ _ -> Builtin.endpointsToSchemas @BettorSchema
        OracleСontract _              -> Builtin.endpointsToSchemas @OracleSchema
    getContract = \case
        MutualBetStartContract params                   -> SomeBuiltin $ mutualBetStart params
        MutualBetBettorContract conf threadToken params -> SomeBuiltin $ mutualBetBettor conf threadToken params
        OracleСontract params                           -> SomeBuiltin $ runOracle params

handlers :: SimulatorEffectHandlers (Builtin MutualBetContracts)
handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (contractHandler (Builtin.handleBuiltin @MutualBetContracts))
