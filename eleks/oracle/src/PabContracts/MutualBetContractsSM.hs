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

module PabContracts.MutualBetContractsSM(
    MutualBetContracts(..)
    , handlers
    ) where

import Contracts.MutualBetSM
import Control.Monad.Freer
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (def))
import Data.OpenApi.Schema qualified as OpenApi
import Data.Row
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.PureScript.Bridge (argonaut, equal, genericShow, mkSumType, order)
import Language.PureScript.Bridge.TypeParameters (A)
import Ledger (TxId)
import Ledger.TimeSlot (SlotConfig)
import Playground.Types (FunctionSchema)
import Plutus.Contract (Contract)
import Plutus.Contract.StateMachine (ThreadToken (..))
import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (..), HasDefinitions (..), SomeBuiltin (..))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Run.PSGenerator (HasPSTypes (..))
import Plutus.PAB.Simulator (SimulatorEffectHandlers)
import Plutus.PAB.Simulator qualified as Simulator
import Prettyprinter
import Schema (FormSchema)
import Types.Game (GameId)

data MutualBetContracts =
    MutualBetOwner MutualBetParams
    | MutualBetUser ThreadToken MutualBetParams
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
    MutualBetOwner _  -> Builtin.endpointsToSchemas @MutualBetStartSchema
    MutualBetUser _ _ -> Builtin.endpointsToSchemas @BettorSchema

getMutualBetContracts :: MutualBetContracts -> SomeBuiltin
getMutualBetContracts = \case
    MutualBetOwner params            -> SomeBuiltin $ mutualBetStart params
    MutualBetUser threadToken params -> SomeBuiltin $ (mutualBetBettor slotCfg threadToken params)

handlers :: SimulatorEffectHandlers (Builtin MutualBetContracts)
handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (contractHandler Builtin.handleBuiltin)

slotCfg :: SlotConfig
slotCfg = def
