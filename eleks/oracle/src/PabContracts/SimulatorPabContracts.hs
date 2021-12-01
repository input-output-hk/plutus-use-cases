{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module PabContracts.SimulatorPabContracts(
    MutualBetContracts(..)
    , handlers
    ) where

import           Control.Monad.Freer
import           Data.Aeson (FromJSON, ToJSON, Result (..))
import           Data.Default (Default (def))
import           GHC.Generics (Generic)
import           Prettyprinter
import           Ledger.TimeSlot (SlotConfig)
import           Language.PureScript.Bridge (argonaut, equal, genericShow, mkSumType, order)
import           Language.PureScript.Bridge.TypeParameters (A)
import           Data.Monoid (Last (..))
import           Data.Row
import qualified Data.OpenApi.Schema as OpenApi
import           Ledger (TxId)
import           Playground.Types (FunctionSchema)
import qualified Plutus.Contract as Contract
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (..), HasDefinitions (..), SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Simulator (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator as Simulator
import           Plutus.PAB.Run.PSGenerator (HasPSTypes (..))
import           Schema (FormSchema)
import           Contracts.Oracle
import           Contracts.MutualBet
import           Plutus.Contracts.Currency as Currency

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
        OracleTokenInit                   -> SomeBuiltin initContract
        MutualBetStartContract params     -> SomeBuiltin $ mutualBetStart params
        MutualBetBettorContract conf threadToken params -> SomeBuiltin $ mutualBetBettor conf threadToken params
        OracleСontract params             -> SomeBuiltin $ runOracle params

handlers :: SimulatorEffectHandlers (Builtin MutualBetContracts)
handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (contractHandler (Builtin.handleBuiltin @MutualBetContracts))

initContract :: Contract.Contract (Last Currency.OneShotCurrency) Currency.CurrencySchema Currency.CurrencyError ()
initContract = do
    ownPK <- Contract.ownPubKeyHash
    cur   <- Currency.mintContract ownPK [("test", 1)]
    let cs = Currency.currencySymbol cur
    Contract.tell $ Last $ Just cur