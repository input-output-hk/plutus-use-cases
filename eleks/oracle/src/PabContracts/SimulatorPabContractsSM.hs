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

import Contracts.Oracle
import Control.Monad.Freer
import Data.Aeson (FromJSON, ToJSON)
import Data.Monoid (Last (..))
import Data.Default (Default (def))
import Data.OpenApi.Schema qualified as OpenApi
import Data.Row
import GHC.Generics (Generic)
import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (..), HasDefinitions (..), SomeBuiltin (..))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Simulator (SimulatorEffectHandlers)
import Plutus.PAB.Simulator qualified as Simulator
import Prettyprinter
import Contracts.MutualBetSM
import Ledger.TimeSlot (SlotConfig)
import Plutus.Contract qualified as Contract
import Plutus.Contracts.Currency as Currency

data MutualBetContracts =
    OracleTokenInit
    | MutualBetStartContract MutualBetParams
    | MutualBetBettorContract SlotConfig ThreadToken MutualBetParams
    | OracleСontract OracleParams
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty MutualBetContracts where
    pretty = viaShow

instance HasDefinitions MutualBetContracts where
    getDefinitions = []
    getSchema = \case
        OracleTokenInit               -> Builtin.endpointsToSchemas @Empty
        MutualBetStartContract _      -> Builtin.endpointsToSchemas @MutualBetStartSchema
        MutualBetBettorContract _ _ _ -> Builtin.endpointsToSchemas @BettorSchema
        OracleСontract _              -> Builtin.endpointsToSchemas @OracleSchema
    getContract = \case
        OracleTokenInit                                 -> SomeBuiltin initContract
        MutualBetStartContract params                   -> SomeBuiltin $ mutualBetStart params
        MutualBetBettorContract conf threadToken params -> SomeBuiltin $ mutualBetBettor conf threadToken params
        OracleСontract params                           -> SomeBuiltin $ runOracle params

handlers :: SimulatorEffectHandlers (Builtin MutualBetContracts)
handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (contractHandler (Builtin.handleBuiltin @MutualBetContracts))

initContract :: Contract.Contract (Last Currency.OneShotCurrency) Currency.CurrencySchema Currency.CurrencyError ()
initContract = do
    ownPK <- Contract.ownPubKeyHash
    cur   <- Currency.mintContract ownPK [("test", 1)]
    Contract.tell $ Last $ Just cur
