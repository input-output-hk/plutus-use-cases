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
import Data.Monoid (Last (..))
import Data.OpenApi.Schema qualified as OpenApi
import Data.Row
import GHC.Generics (Generic)
import Plutus.Contract qualified as Contract
import Plutus.Contracts.Currency as Currency
import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (..), HasDefinitions (..), SomeBuiltin (..))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Simulator (SimulatorEffectHandlers)
import Plutus.PAB.Simulator qualified as Simulator
import Prettyprinter

data MutualBetContracts =
    OracleTokenInit
    | MutualBetStartContract MutualBetStartParams
    | MutualBetBettorContract MutualBetParams
    | OracleСontract OracleParams
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty MutualBetContracts where
    pretty = viaShow

instance HasDefinitions MutualBetContracts where
    getDefinitions = []
    getSchema = \case
        OracleTokenInit           -> Builtin.endpointsToSchemas @Empty
        MutualBetStartContract _  -> Builtin.endpointsToSchemas @Empty
        MutualBetBettorContract _ -> Builtin.endpointsToSchemas @BettorSchema
        OracleСontract _          -> Builtin.endpointsToSchemas @OracleSchema
    getContract = \case
        OracleTokenInit                -> SomeBuiltin initContract
        MutualBetStartContract params  -> SomeBuiltin $ mutualBetStartWithOracle params
        MutualBetBettorContract params -> SomeBuiltin $ mutualBetBettor params
        OracleСontract params          -> SomeBuiltin $ runOracle params

handlers :: SimulatorEffectHandlers (Builtin MutualBetContracts)
handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (contractHandler (Builtin.handleBuiltin @MutualBetContracts))

initContract :: Contract.Contract (Last Currency.OneShotCurrency) Currency.CurrencySchema Currency.CurrencyError ()
initContract = do
    ownPK <- Contract.ownPubKeyHash
    cur   <- Currency.mintContract ownPK [("test", 1)]
    Contract.tell $ Last $ Just cur
