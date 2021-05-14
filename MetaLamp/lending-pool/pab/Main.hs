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

import           Control.Monad                       (forM, forM_, void, when)
import           Control.Monad.Freer                 (Eff, Member, interpret,
                                                      type (~>))
import           Control.Monad.Freer.Error           (Error)
import           Control.Monad.Freer.Extras.Log      (LogMsg)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (FromJSON, Result (..),
                                                      ToJSON, encode, fromJSON)
import qualified Data.Map.Strict                     as Map
import qualified Data.Monoid                         as Monoid
import qualified Data.Semigroup                      as Semigroup
import           Data.Text                           (Text)
import           Data.Text.Prettyprint.Doc           (Pretty (..), viaShow)
import           GHC.Generics                        (Generic)
import           Ledger
import           Ledger.Ada                          (adaSymbol, adaToken,
                                                      adaValueOf,
                                                      lovelaceValueOf)
import           Ledger.Constraints
import           Ledger.Value                        as Value
import           Plutus.Contract                     hiding (when)
import qualified Plutus.Contracts.Currency           as Currency
import qualified Plutus.Contracts.LendingPool        as Aave
import           Plutus.PAB.Effects.Contract         (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..),
                                                      type (.\\))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Monitoring.PABLogMsg     (PABMultiAgentMsg)
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import           Plutus.PAB.Types                    (PABError (..))
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Prelude                             hiding (init)
import           Wallet.Emulator.Types               (Wallet (..), walletPubKey)

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
    Simulator.logString @(Builtin AaveContracts) "Starting Aave PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    cidInit  <- Simulator.activateContract (Wallet 1) Init
    _        <- Simulator.waitUntilFinished cidInit

    Simulator.logString @(Builtin AaveContracts) "Initialization finished."

    cidStart <- Simulator.activateContract (Wallet 1) AaveStart
    aa       <- flip Simulator.waitForState cidStart $ \json -> case (fromJSON json :: Result (Monoid.Last (Either Text Aave.Aave))) of
                    Success (Monoid.Last (Just (Right aa))) -> Just aa
                    _                                       -> Nothing
    Simulator.logString @(Builtin AaveContracts) $ "Aave instance created: " ++ show aa

    cids <- fmap Map.fromList $ forM wallets $ \w -> do
        cid <- Simulator.activateContract w $ AaveUser aa
        Simulator.logString @(Builtin AaveContracts) $ "Aave user contract started for " ++ show w
        return (w, cid)

    Simulator.logString @(Builtin AaveContracts) "creating liquidity pool"
    _  <- Simulator.callEndpointOnInstance (cids Map.! Wallet 2) "create" ()
    flip Simulator.waitForState (cids Map.! Wallet 2) $ \json -> case (fromJSON json :: Result (Monoid.Last (Either Text Aave.UserContractState))) of
        Success (Monoid.Last (Just (Right Aave.Created))) -> Just ()
        _                                                 -> Nothing
    Simulator.logString @(Builtin AaveContracts) "liquidity pool created"

    _ <- liftIO getLine
    shutdown

data AaveContracts =
      Init
    | AaveStart
    | AaveUser Aave.Aave
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty AaveContracts where
    pretty = viaShow

handleAaveContract ::
    ( Member (Error PABError) effs
    , Member (LogMsg (PABMultiAgentMsg (Builtin AaveContracts))) effs
    )
    => ContractEffect (Builtin AaveContracts)
    ~> Eff effs
handleAaveContract = Builtin.handleBuiltin getSchema getContract where
  getSchema = \case
    AaveUser _ -> Builtin.endpointsToSchemas @(Aave.AaveUserSchema .\\ BlockchainActions)
    AaveStart  -> Builtin.endpointsToSchemas @(Aave.AaveOwnerSchema .\\ BlockchainActions)
    Init          -> Builtin.endpointsToSchemas @Empty
  getContract = \case
    AaveUser us -> SomeBuiltin $ Aave.userEndpoints us
    AaveStart   -> SomeBuiltin Aave.ownerEndpoint
    Init        -> SomeBuiltin initContract

handlers :: SimulatorEffectHandlers (Builtin AaveContracts)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin AaveContracts) [] -- [Init, AaveStart, AaveUser ???]
    $ interpret handleAaveContract
