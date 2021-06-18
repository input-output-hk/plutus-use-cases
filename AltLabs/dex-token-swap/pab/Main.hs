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
{-# LANGUAGE ScopedTypeVariables      #-}

module Main
    ( main
    ) where

import           Control.Monad                           (forM, void)
import           Control.Monad.Freer                     (Eff, Member, interpret, type (~>))
import           Control.Monad.Freer.Error               (Error)
import           Control.Monad.Freer.Extras.Log          (LogMsg)
import           Control.Monad.IO.Class                  (MonadIO (..))
import qualified Data.Aeson.Encode.Pretty                as JSON
import           Data.Aeson                              (FromJSON, Result (..), ToJSON, encode, fromJSON)
import qualified Data.Map.Strict                         as Map
import qualified Data.Monoid                             as Monoid
import qualified Data.Semigroup                          as Semigroup
import           Data.Text                               (Text)
import           GHC.Generics                            (Generic)
import           Ledger.Ada                              (adaSymbol, adaToken)
import           Plutus.Contract
import qualified Plutus.Contracts.Currency               as Currency
import qualified Plutus.Contracts.Uniswap                as Uniswap
import           Plutus.PAB.Effects.Contract             (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin     (Builtin, SomeBuiltin (..), type (.\\))
import qualified Plutus.PAB.Effects.Contract.Builtin     as Builtin
import           Effects.Uniswap as US
import           Plutus.PAB.Monitoring.PABLogMsg         (PABMultiAgentMsg)
import Plutus.PAB.Simulator ( SimulatorEffectHandlers, Simulation )
import qualified Plutus.PAB.Simulator                    as Simulator
import           Plutus.PAB.Types                        (PABError (..))
import qualified Plutus.PAB.Webserver.Server             as PAB.Server
import qualified Plutus.PAB.Webserver.Handler            as Webserver
import           Plutus.PAB.Webserver.Types              (ContractSignatureResponse, FullReport)
import           Prelude                                 hiding (init)
import           Wallet.Emulator.Types                   (Wallet (..))
import qualified Data.ByteString.Lazy                    as BSL
import           Data.Text.Prettyprint.Doc
import qualified Data.Text                                  as Text
import           Wallet.Types                               (ContractInstanceId (..))
import           Playground.Types                            (FunctionSchema, Simulation)
import           Schema                                      (FormSchema)
import System.Directory                                  (getCurrentDirectory)
import System.FilePath                                   ((</>))
import Plutus.PAB.Core                                   (PABEffects, PABAction)
import Ledger                                            (CurrencySymbol)
import Plutus.Contracts.Data

import qualified Control.Concurrent.STM          as STM
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import Control.Concurrent (threadDelay, MVar, newEmptyMVar, putMVar, tryTakeMVar)
import Data.Maybe (isJust)

uniswapMintingContract :: Eff (PABEffects
     (Builtin UniswapContracts)
     (Simulator.SimulatorState (Builtin UniswapContracts)))
  (ContractInstanceId, CurrencySymbol)
uniswapMintingContract = do
    cidInit  <- Simulator.activateContract (Wallet 1) Init
    cs       <- flip Simulator.waitForState cidInit $ \json -> case fromJSON json of
                    Success (Just (Semigroup.Last cur)) -> Just $ Currency.currencySymbol cur
                    _                                   -> Nothing
    _        <- Simulator.waitUntilFinished cidInit

    Simulator.logString @(Builtin UniswapContracts) $ "Initialization finished. Minted: " ++ show cs

    return (cidInit, cs)

uniswapStartContract :: Eff (PABEffects
     (Builtin UniswapContracts)
     (Simulator.SimulatorState (Builtin UniswapContracts)))
  (ContractInstanceId, Uniswap.Uniswap)
uniswapStartContract = do
    cidStart <- Simulator.activateContract (Wallet 1) UniswapStart
    us       <- flip Simulator.waitForState cidStart $ \json -> case (fromJSON json :: Result (Monoid.Last (Either Text Uniswap.Uniswap))) of
                    Success (Monoid.Last (Just (Right us))) -> Just us
                    _                                       -> Nothing
    Simulator.logString @(Builtin UniswapContracts) $ "Uniswap instance created: " ++ show us

    pure (cidStart, us)

uniswapUserContract us =
    fmap Map.fromList $ forM wallets $ \w -> do
    cid <- Simulator.activateContract w $ UniswapUser us
    Simulator.logString @(Builtin UniswapContracts) $ "Uniswap user contract started for " ++ show w
    Simulator.waitForEndpoint cid "funds"
    _ <- Simulator.callEndpointOnInstance cid "funds" ()
    v <- flip Simulator.waitForState cid $ \json -> case (fromJSON json :: Result (Monoid.Last (Either Text Uniswap.UserContractState))) of
            Success (Monoid.Last (Just (Right (Uniswap.Funds v)))) -> Just v
            _                                                      -> Nothing
    Simulator.logString @(Builtin UniswapContracts) $ "initial funds in wallet " ++ show w ++ ": " ++ show v
    return (w, cid)

uniswapLiquidityPoolContract cids cs = do
    let coins = Map.fromList [(tn, Uniswap.mkCoin cs tn) | tn <- tokenNames]
        ada   = Uniswap.mkCoin adaSymbol adaToken

    let cp = CreateParams ada (coins Map.! "A") 100000 500000
    Simulator.logString @(Builtin UniswapContracts) $ "creating liquidity pool: " ++ show (encode cp)
    -- _  <- Simulator.callEndpointOnInstance (cids Map.! Wallet 2) "create" cp
    let cid2 = cids Map.! Wallet 2
    Simulator.waitForEndpoint cid2 "create"

    _  <- Simulator.callEndpointOnInstance cid2 "create" cp
    flip Simulator.waitForState (cids Map.! Wallet 2) $ \json -> case (fromJSON json :: Result (Monoid.Last (Either Text Uniswap.UserContractState))) of
        Success (Monoid.Last (Just (Right Uniswap.Created))) -> Just ()
        _                                                    -> Nothing

    Simulator.logString @(Builtin UniswapContracts) "liquidity pool created"

startDebugServer :: MVar ()
    -> Eff
     (PABEffects
        (Builtin UniswapContracts)
        (Simulator.SimulatorState (Builtin UniswapContracts)))
     (FullReport UniswapContracts,
      ContractSignatureResponse UniswapContracts)
startDebugServer stop = do
    Simulator.logString @(Builtin UniswapContracts) "Starting Uniswap PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    -- UNI: MINT
    (cidInit, cs) <- uniswapMintingContract

    -- UNI: START
    (cidStart, us) <- uniswapStartContract

    -- UNI: USER
    cids <- uniswapUserContract us

    -- UNI: POOL
    void (uniswapLiquidityPoolContract cids cs)

    _ <- liftIO $ loop stop
    shutdown

    report :: FullReport UniswapContracts <- Webserver.getFullReport
    schema :: ContractSignatureResponse UniswapContracts <- Webserver.contractSchema (Text.pack $ show $ unContractInstanceId cidInit)
    pure (report, schema)

startServer :: MVar () -> IO
  (FullReport UniswapContracts,
   ContractSignatureResponse UniswapContracts)
startServer stop =
    either (error . show) id <$> Simulator.runSimulationWith handlers (startDebugServer stop)

onSigInt :: MVar () -> IO ()
onSigInt stop = do
  putStrLn "got sigINT"
  putMVar stop ()

onSigTerm :: MVar () -> IO ()
onSigTerm stop = do
  putStrLn "got sigTERM"
  putMVar stop ()

main :: IO ()
main = do
    stop <- newEmptyMVar
    _ <- installHandler sigINT (Catch $ onSigInt stop) Nothing
    _ <- installHandler sigTERM (Catch $ onSigTerm stop) Nothing
    
    (fullReport, currencySchema) <- startServer stop

    outputDir <- getCurrentDirectory
    print outputDir
    BSL.writeFile
        (outputDir </> "./log/full_report_response.json")
        (JSON.encodePretty fullReport)

    print outputDir
    BSL.writeFile
        (outputDir </> "./log/currency_schema.json")
        (JSON.encodePretty currencySchema)

loop :: MVar () -> IO ()
loop stop = do
  stopRequested <- isJust <$> tryTakeMVar stop
  if stopRequested
    then putStrLn "stop :: MVar — mutated! halting..."
    else do
      threadDelay 1000000
      loop stop

data UniswapContracts =
      Init
    | UniswapStart
    | UniswapUser Uniswap.Uniswap
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty UniswapContracts where
    pretty = viaShow

handleUniswapContract ::
    ( Member (Error PABError) effs
    , Member (LogMsg (PABMultiAgentMsg (Builtin UniswapContracts))) effs
    )
    => ContractEffect (Builtin UniswapContracts)
    ~> Eff effs
handleUniswapContract = Builtin.handleBuiltin getSchema getContract where
  getSchema = \case
    UniswapUser _ -> Builtin.endpointsToSchemas @(Uniswap.UniswapUserSchema .\\ BlockchainActions)
    UniswapStart  -> Builtin.endpointsToSchemas @(Uniswap.UniswapOwnerSchema .\\ BlockchainActions)
    Init          -> Builtin.endpointsToSchemas @Empty
  getContract = \case
    UniswapUser us -> SomeBuiltin $ Uniswap.userEndpoints us
    UniswapStart   -> SomeBuiltin Uniswap.ownerEndpoint
    Init           -> SomeBuiltin US.initContract

handlers :: SimulatorEffectHandlers (Builtin UniswapContracts)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin UniswapContracts) [] -- [Init, UniswapStart, UniswapUser ???]
    $ interpret handleUniswapContract