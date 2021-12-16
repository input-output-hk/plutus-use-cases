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
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.PureScript.Bridge (argonaut, equal, genericShow, mkSumType)
import Plutus.Contract (Contract)
import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (..), HasDefinitions (..), SomeBuiltin (..))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Run.PSGenerator (HasPSTypes (..))
import Plutus.PAB.Simulator (SimulatorEffectHandlers)
import Plutus.PAB.Simulator qualified as Simulator
import Prettyprinter

data MutualBetContracts =
    MutualBetStartContract MutualBetStartParams
    | MutualBetBettorContract MutualBetParams
    | OracleContract OracleParams
    | OracleRequest Oracle
    | OracleRedeemRequest Oracle
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty MutualBetContracts where
    pretty = viaShow

instance HasPSTypes MutualBetContracts where
    psTypes =
        [ equal . genericShow . argonaut $ mkSumType @MutualBetContracts
        ]

instance HasDefinitions MutualBetContracts where
    getDefinitions = []
    getSchema = \case
        MutualBetStartContract _  -> Builtin.endpointsToSchemas @Empty
        MutualBetBettorContract _ -> Builtin.endpointsToSchemas @BettorSchema
        OracleContract _          -> Builtin.endpointsToSchemas @OracleSchema
        OracleRequest _           -> Builtin.endpointsToSchemas @Empty
        OracleRedeemRequest _     -> Builtin.endpointsToSchemas @RedeemOracleSchema
    getContract = \case
        MutualBetStartContract params  -> SomeBuiltin $ mutualBetStartWithOracle params
        MutualBetBettorContract params -> SomeBuiltin $ mutualBetBettor params
        OracleContract params          -> SomeBuiltin $ runOracle params
        OracleRequest oracle           -> SomeBuiltin $ (requestOracleForAddress oracle 1 :: Contract () Empty Text ())
        OracleRedeemRequest oracle     -> SomeBuiltin $ (redeemOracle oracle)

handlers :: SimulatorEffectHandlers (Builtin MutualBetContracts)
handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (contractHandler (Builtin.handleBuiltin @MutualBetContracts))
