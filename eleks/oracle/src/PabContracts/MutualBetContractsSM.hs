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

module PabContracts.MutualBetContractsSM(
    MutualBetContracts(..)
    , handlers
    ) where

import           Control.Monad.Freer
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Default (Default (def))
import           GHC.Generics (Generic)
import           Prettyprinter
import           Data.Default                       (Default (def))
import           Language.PureScript.Bridge (argonaut, equal, genericShow, mkSumType, order)
import           Language.PureScript.Bridge.TypeParameters (A)
import           Ledger.TimeSlot (SlotConfig)
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
import           Contracts.MutualBetSM
import           Plutus.Contract (Contract)
import           Types.Game (GameId)
import           Plutus.Contract.StateMachine     (ThreadToken(..))

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
    MutualBetOwner _    -> Builtin.endpointsToSchemas @MutualBetStartSchema
    MutualBetUser _ _ -> Builtin.endpointsToSchemas @BettorSchema

getMutualBetContracts :: MutualBetContracts -> SomeBuiltin
getMutualBetContracts = \case
    MutualBetOwner params -> SomeBuiltin $ mutualBetStart params
    MutualBetUser threadToken params -> SomeBuiltin $ (mutualBetBettor slotCfg threadToken params)

handlers :: SimulatorEffectHandlers (Builtin MutualBetContracts)
handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (contractHandler Builtin.handleBuiltin)

slotCfg :: SlotConfig
slotCfg = def