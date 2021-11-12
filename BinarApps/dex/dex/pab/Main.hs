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
{-# LANGUAGE ViewPatterns       #-}
module Main
  ( main
  ) where

import           Control.Monad                       (forM, void)
import           Control.Monad.Freer                 (interpret)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (FromJSON (..), ToJSON (..), Value)
import           Data.Aeson.Types                    (parseMaybe)
import           Data.Default                        (Default (def))
import qualified Data.Map                            as Map
import           Data.OpenApi.Internal.Schema        (ToSchema)
import qualified Data.Semigroup                      as Semigroup
import           Data.Text                           (Text)
import           Data.Text.Prettyprint.Doc           (Pretty (..), viaShow)
import qualified Dex.OffChain                        as Dex
import qualified Dex.Trace                           as Trace
import           Dex.Types                           (DexContractState)
import qualified Dex.Types                           as Dex
import qualified Dex.WalletHistory                   as WH
import           GHC.Generics                        (Generic)
import           Plutus.Contract                     (Empty)
import qualified Plutus.Contracts.Currency           as Currency
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Simulator                (Simulation, SimulatorEffectHandlers, logString)
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Wallet.Emulator.Types               (knownWallet)
import           Wallet.Types                        (ContractInstanceId)

type ContractHistory = WH.History (Either Text Dex.DexContractState)

main :: IO ()
main = void $
  Simulator.runSimulationWith handlers $ do
    logString @(Builtin DexContracts) "Starting Uniswap PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    cidInit <- Simulator.activateContract (knownWallet 1) DexInit
    cs <- getState (Currency.currencySymbol . Semigroup.getLast) cidInit

    void $ Simulator.waitUntilFinished cidInit

    logString @(Builtin DexContracts) $ "Initialization finished. Minted: " ++ show cs

    void $ fmap Map.fromList $
      forM Trace.wallets $ \w -> do
        cid <- Simulator.activateContract w DexContract
        logString @(Builtin DexContracts) $ "Uniswap user contract started for " ++ show w
        Simulator.waitForEndpoint cid "funds"
        void $ Simulator.callEndpointOnInstance cid "funds" (Dex.Request "FundsId" 0 ())
        v <- getState (contractState "FundsId") cid
        logString @(Builtin DexContracts) $ "initial funds in wallet " ++ show w ++ ": " ++ show v
        return (w, cid)

    void $ liftIO getLine
    shutdown
  where
    fromJSONValue :: FromJSON a => Value -> Maybe a
    fromJSONValue = parseMaybe parseJSON

    getState :: (FromJSON a) => (a -> b) -> ContractInstanceId -> Simulation t b
    getState f = Simulator.waitForState (fmap f . fromJSONValue)

    contractState :: Text -> ContractHistory -> Maybe DexContractState
    contractState historyId (WH.lookup historyId -> Just (Right v)) = Just v
    contractState _ _                                               = Nothing


data DexContracts = DexContract | DexInit deriving (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

instance Pretty DexContracts where
  pretty = viaShow

instance Builtin.HasDefinitions DexContracts where
    getDefinitions = [DexContract]
    getSchema = \case
      DexContract -> Builtin.endpointsToSchemas @Dex.DexSchema
      DexInit     -> Builtin.endpointsToSchemas @Empty
    getContract = \case
      DexContract -> SomeBuiltin Dex.dexEndpoints
      DexInit     -> SomeBuiltin Trace.setupTokens

handlers :: SimulatorEffectHandlers (Builtin DexContracts)
handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (Builtin.contractHandler (Builtin.handleBuiltin @DexContracts))
