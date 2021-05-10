{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
module Main
    ( main
    ) where

import Control.Monad ( forM, void, forM_, when )
import           Control.Monad.Freer                     (Eff, Member, interpret, type (~>))
import           Control.Monad.Freer.Error               (Error)
import           Control.Monad.Freer.Extras.Log          (LogMsg)
import           Control.Monad.IO.Class                  (MonadIO (..))
import           Data.Aeson                              (FromJSON, Result (..), ToJSON, encode, fromJSON)
import qualified Data.Map.Strict                         as Map
import qualified Data.Monoid                             as Monoid
import           Data.Text                               (Text)
import           Data.Text.Prettyprint.Doc               (Pretty (..), viaShow)
import           GHC.Generics                            (Generic)
import           Ledger.Ada                              (adaSymbol, adaToken, adaValueOf,lovelaceValueOf)
import qualified Plutus.Contracts.LendingPool                as Todo
import           Plutus.PAB.Effects.Contract             (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin     (Builtin, SomeBuiltin (..), type (.\\))
import qualified Plutus.PAB.Effects.Contract.Builtin     as Builtin
import           Plutus.PAB.Monitoring.PABLogMsg         (PABMultiAgentMsg)
import           Plutus.PAB.Simulator                    (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                    as Simulator
import           Plutus.PAB.Types                        (PABError (..))
import qualified Plutus.PAB.Webserver.Server             as PAB.Server
import           Prelude                                 hiding (init)
import qualified Data.Semigroup            as Semigroup
import           Ledger
import           Ledger.Constraints
import           Ledger.Value              as Value
import           Plutus.Contract           hiding (when)
import qualified Plutus.Contracts.Currency as Currency
import           Wallet.Emulator.Types     (Wallet (..), walletPubKey)

initContract :: Contract (Maybe (Semigroup.Last Currency.OneShotCurrency)) Currency.CurrencySchema Currency.CurrencyError ()
initContract = do
    ownPK <- pubKeyHash <$> ownPubKey
    let v  = lovelaceValueOf amount
    forM_ wallets $ \w -> do
        let pkh = pubKeyHash $ walletPubKey w
        when (pkh /= ownPK) $ do
            tx <- submitTx $ mustPayToPubKey pkh v
            awaitTxConfirmed $ txId tx
  where
    amount = 1000000

wallets :: [Wallet]
wallets = [Wallet i | i <- [1 .. 4]]

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin TodoContracts) "Starting Todo PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    cidInit  <- Simulator.activateContract (Wallet 1) Init
    _        <- Simulator.waitUntilFinished cidInit

    Simulator.logString @(Builtin TodoContracts) "Initialization finished."

    cidStart <- Simulator.activateContract (Wallet 1) TodoStart
    aa       <- flip Simulator.waitForState cidStart $ \json -> case (fromJSON json :: Result (Monoid.Last (Either Text Todo.Todo))) of
                    Success (Monoid.Last (Just (Right aa))) -> Just aa
                    _                                       -> Nothing

    cids <- fmap Map.fromList $ forM wallets $ \w -> do
        cid <- Simulator.activateContract w $ TodoUser aa
        return (w, cid)

    let cp = 10000 :: Integer
    _  <- Simulator.callEndpointOnInstance (cids Map.! Wallet 2) "mint" cp
    flip Simulator.waitForState (cids Map.! Wallet 2) $ \json -> case (fromJSON json :: Result (Monoid.Last (Either Text Todo.UserContractState))) of
        Success (Monoid.Last (Just (Right Todo.Created))) -> Just ()
        _                                                    -> Nothing

    _ <- liftIO getLine
    shutdown

data TodoContracts =
      Init
    | TodoStart
    | TodoUser Todo.Todo
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty TodoContracts where
    pretty = viaShow

handleTodoContract ::
    ( Member (Error PABError) effs
    , Member (LogMsg (PABMultiAgentMsg (Builtin TodoContracts))) effs
    )
    => ContractEffect (Builtin TodoContracts)
    ~> Eff effs
handleTodoContract = Builtin.handleBuiltin getSchema getContract where
  getSchema = \case
    TodoUser _ -> Builtin.endpointsToSchemas @(Todo.TodoUserSchema .\\ BlockchainActions)
    TodoStart  -> Builtin.endpointsToSchemas @(Todo.TodoOwnerSchema .\\ BlockchainActions)
    Init          -> Builtin.endpointsToSchemas @Empty
  getContract = \case
    TodoUser us -> SomeBuiltin $ Todo.userEndpoints us
    TodoStart   -> SomeBuiltin Todo.ownerEndpoint
    Init           -> SomeBuiltin initContract

handlers :: SimulatorEffectHandlers (Builtin TodoContracts)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin TodoContracts) [] -- [Init, TodoStart, TodoUser ???]
    $ interpret handleTodoContract
