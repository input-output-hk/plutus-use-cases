{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Plutus.PAB.App where

import           Control.Monad                                        (forM,
                                                                       forM_,
                                                                       void,
                                                                       when)
import           Control.Monad.Freer                                  (Eff,
                                                                       Member,
                                                                       interpret,
                                                                       type (~>))
import           Control.Monad.Freer.Error                            (Error)
import           Control.Monad.Freer.Extras.Log                       (LogMsg)
import           Control.Monad.IO.Class                               (MonadIO,
                                                                       liftIO)
import           Data.Aeson                                           (FromJSON,
                                                                       Result (..),
                                                                       ToJSON,
                                                                       encode,
                                                                       fromJSON)
import qualified Data.Aeson                                           as JSON
import qualified Data.ByteString                                      as BS
import qualified Data.Map.Strict                                      as Map
import qualified Data.Monoid                                          as Monoid
import qualified Data.Semigroup                                       as Semigroup
import           Data.Text                                            (Text)
import qualified Data.Text                                            as T
import           Data.Text.Prettyprint.Doc                            (Pretty (..),
                                                                       viaShow)
import           GHC.Generics                                         (Generic)
import           Ledger
import           Ledger.Ada                                           (adaSymbol,
                                                                       adaToken,
                                                                       adaValueOf,
                                                                       lovelaceValueOf)
import           Ledger.Constraints
import qualified Ledger.Constraints.OffChain                          as Constraints
import qualified Ledger.Typed.Scripts                                 as Scripts
import           Ledger.Value                                         as Value
import           Plutus.Abstract.ContractResponse                     (ContractResponse (..))
import           Plutus.Contract                                      hiding
                                                                      (when)
import           Plutus.Contracts.Currency                            as Currency
import qualified Plutus.Contracts.LendingPool.OffChain.Info           as Aave
import qualified Plutus.Contracts.LendingPool.OffChain.Owner          as Aave
import qualified Plutus.Contracts.LendingPool.OffChain.User           as Aave
import qualified Plutus.Contracts.LendingPool.OnChain.Core            as Aave
import qualified Plutus.Contracts.Service.FungibleToken               as FungibleToken
import qualified Plutus.Contracts.Service.Oracle                      as Oracle
import           Plutus.PAB.Effects.Contract                          (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin                  (Builtin,
                                                                       SomeBuiltin (..),
                                                                       type (.\\))
import qualified Plutus.PAB.Effects.Contract.Builtin                  as Builtin
import           Plutus.PAB.Simulator                                 (Simulation,
                                                                       SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                                 as Simulator
import           Plutus.PAB.Types                                     (PABError (..))
import qualified Plutus.PAB.Webserver.Server                          as PAB.Server
import           Plutus.V1.Ledger.Crypto                              (getPubKeyHash,
                                                                       pubKeyHash)
import           Prelude                                              hiding
                                                                      (init)
import           Wallet.Emulator.Types                                (Wallet (..),
                                                                       walletPubKey)
import           Wallet.Types                                         (ContractInstanceId)

import qualified Cardano.BM.Configuration.Model                       as CM
import           Cardano.BM.Data.Trace                                (Trace)
import           Cardano.BM.Setup                                     (setupTrace_)
import           Control.Concurrent                                   (takeMVar)
import           Control.Concurrent.Availability                      (newToken)
import qualified Control.Monad.Freer.Extras.Log                       as Log
import           Data.Foldable                                        (for_)
import           Data.Yaml                                            (decodeFileThrow)
import qualified "plutus-pab" Plutus.PAB.App                          as App
import qualified "plutus-pab" Plutus.PAB.Core                         as Core
import qualified Plutus.PAB.Effects.Contract                          as Contract
import           "plutus-pab" Plutus.PAB.Effects.Contract.ContractExe
import           Plutus.PAB.Monitoring.Config                         (defaultConfig,
                                                                       loadConfig)
import qualified Plutus.PAB.Monitoring.Monitoring                     as LM
import           Plutus.PAB.Monitoring.PABLogMsg
import           Plutus.PAB.Monitoring.Util                           (PrettyObject (..),
                                                                       convertLog)
import           Plutus.PAB.Simulation
import           "plutus-pab" Plutus.PAB.Types
import qualified "plutus-pab" Plutus.PAB.Webserver.Server             as PABServer

defaultContracts :: [ContractExe]
defaultContracts =
    [ ContractExe distributeFundsExe
    , ContractExe createOraclesExe
    , ContractExe aaveStartExe
    , ContractExe aaveUserExe
    , ContractExe aaveInfoExe
    ]

distributeFundsExe :: FilePath
distributeFundsExe = "./contracts/distribute-funds"

createOraclesExe :: FilePath
createOraclesExe = "./contracts/create-oracles"

aaveStartExe :: FilePath
aaveStartExe = "./contracts/aave-start"

aaveUserExe :: FilePath
aaveUserExe = "./contracts/aave-user"

aaveInfoExe :: FilePath
aaveInfoExe = "./contracts/aave-info"

runApp :: IO (Either PABError ())
runApp = do
    let eventfulBackend = App.SqliteBackend
    logConfig <- defaultConfig
    (trace :: Trace IO (PrettyObject (AppMsg ContractExe)), switchboard) <- setupTrace_ logConfig "pab"

    serviceAvailability <- newToken

    config@Config{..} <- decodeFileThrow "./plutus-pab.yaml"
    let trace' = toPABMsg $ convertLog PrettyObject trace
    App.migrate trace' dbConfig
    App.runApp eventfulBackend trace' config $ do
        App.AppEnv{App.walletClientEnv} <- Core.askUserEnv @ContractExe @App.AppEnv
        (mvar, _) <- PABServer.startServer pabWebserverConfig (Left walletClientEnv) serviceAvailability
        _ <- installContracts
        _ <- fillWithAssets
        liftIO $ getLine
        liftIO $ takeMVar mvar
  where
    toPABMsg = LM.convertLog LM.PABMsg

    installContracts = for_ defaultContracts $ Contract.addDefinition @ContractExe

fillWithAssets = do
    cidFunds <- Core.activateContract ownerWallet $ ContractExe distributeFundsExe
    res <- Core.waitUntilFinished cidFunds
    Log.logDebug @(PABMultiAgentMsg ContractExe) $ UserLog $ "Final Results: " <> (T.pack $ show res)

    cidOracles <- Core.activateContract ownerWallet $ ContractExe createOraclesExe
    oracles <- flip Core.waitForState cidOracles $ \json -> case (fromJSON json :: Result (∏Monoid.Last [Oracle.Oracle])) of
        Success (Monoid.Last (Just res)) -> Just res
        _                                -> Nothing
    Log.logDebug @(PABMultiAgentMsg ContractExe) $ UserLog $ "Initialization finished. Created oracles: " <> (T.pack $ show oracles)

    cidStart <- Core.activateContract ownerWallet $ ContractExe aaveStartExe
    _ <- Core.callEndpointOnInstance cidStart "start" $ fmap (\o -> Aave.CreateParams (Oracle.oAsset o) o) oracles
    aa <- flip Core.waitForState cidStart $ \json -> case (fromJSON json :: Result (ContractResponse Text Aave.OwnerContractState)) of
        Success (ContractSuccess (Aave.Started aa)) -> Just aa
        _                                           -> Nothing
    liftIO $ writeFile "aa.aave" $ show $ JSON.encode aa -- TODO: Temporary way. Needs to be reworked.
    Log.logDebug @(PABMultiAgentMsg ContractExe) $ UserLog $ "Aave instance created: " <> (T.pack $ show aa)

    cidInfo <- Core.activateContract ownerWallet $ ContractExe aaveInfoExe

    cidUser <- fmap Map.fromList $
        forM userWallets $ \w -> do
            cid <- Core.activateContract w $ ContractExe aaveUserExe
            Log.logDebug @(PABMultiAgentMsg ContractExe) $ UserLog $ "Aave user contract started for " <> (T.pack $ show w)
            return (w, cid)

    pure $ ContractIDs cidUser cidInfo
