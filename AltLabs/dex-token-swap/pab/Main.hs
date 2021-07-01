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
import           Data.Text.Encoding                      ( encodeUtf8 )
import           GHC.Generics                            (Generic)
import           Ledger.Ada                              (adaSymbol, adaToken)
import           Plutus.Contract
import qualified Plutus.Contracts.Currency               as Currency
import qualified Plutus.Contracts.Uniswap                as Uniswap
import           Plutus.PAB.Effects.Contract             (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin     (Builtin, SomeBuiltin (..))
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
import qualified Data.ByteString.Encoding                as BSE
import qualified Data.ByteString                         as BS
import           Data.Text.Prettyprint.Doc
import qualified Data.Text                                  as Text
import           Wallet.Types                               (ContractInstanceId (..))
import           Playground.Types                            (FunctionSchema, Simulation)
import           Schema                                      (FormSchema)
import System.Directory                                  (getCurrentDirectory)
import System.FilePath                                   ((</>))
import Plutus.PAB.Core                                   (PABEffects, PABAction)
import Ledger                                            (CurrencySymbol)
import Ledger.Value                                      (unCurrencySymbol)
import Plutus.Contracts.Data

import qualified Control.Concurrent.STM          as STM
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import Control.Concurrent (threadDelay, MVar, newEmptyMVar, putMVar, tryTakeMVar)
import Data.Maybe (isJust)
import SimulatorActions
-- import Control.Lens.Indexed (ifor_)
import Data.Foldable (for_)
import Data.Map (Map,toList)

cidFile :: Wallet -> FilePath
cidFile w = "./tmp/W" ++ show (getWallet w) ++ ".cid"

cidsToFile :: Map Wallet ContractInstanceId -> IO ()
cidsToFile cids = do
  for_ (toList cids) $ \(w, cid) -> do
    let text = Text.pack $ show $ unContractInstanceId cid
    let bs = BSE.encode BSE.utf8 text
    BS.writeFile (cidFile w) bs

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
    -- liftIO $ BSL.writeFile "./tmp/symbol.json" $ encode cs
    liftIO $ BS.writeFile "./tmp/symbol" $ BSE.encode BSE.utf8 $ Text.pack $ show cs

    -- UNI: START
    (cidStart, us) <- uniswapStartContract

    -- UNI: USER
    cids <- uniswapUserContract us
    liftIO $ cidsToFile cids

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

handleUniswapContract ::
    ( Member (Error PABError) effs
    , Member (LogMsg (PABMultiAgentMsg (Builtin UniswapContracts))) effs
    )
    => ContractEffect (Builtin UniswapContracts)
    ~> Eff effs
handleUniswapContract = Builtin.handleBuiltin getSchema getContract where
  getSchema = \case
    UniswapUser _ -> Builtin.endpointsToSchemas @Uniswap.UniswapUserSchema
    UniswapStart  -> Builtin.endpointsToSchemas @Uniswap.UniswapOwnerSchema
    Init          -> Builtin.endpointsToSchemas @Empty
  getContract = \case
    UniswapUser us -> SomeBuiltin $ Uniswap.userEndpoints us
    UniswapStart   -> SomeBuiltin Uniswap.ownerEndpoint
    Init           -> SomeBuiltin US.initContract

handlers :: SimulatorEffectHandlers (Builtin UniswapContracts)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin UniswapContracts) [] -- [Init, UniswapStart, UniswapUser ???]
    $ interpret handleUniswapContract