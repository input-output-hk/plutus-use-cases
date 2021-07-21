{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Plutus.PAB.Simulation where

import           Control.Monad                               (forM, forM_, void,
                                                              when)
import           Control.Monad.Freer                         (Eff, Member,
                                                              interpret,
                                                              type (~>))
import           Control.Monad.Freer.Error                   (Error)
import           Control.Monad.Freer.Extras.Log              (LogMsg)
import           Control.Monad.IO.Class                      (MonadIO (..))
import           Data.Aeson                                  (FromJSON,
                                                              Result (..),
                                                              ToJSON, encode,
                                                              fromJSON)
import qualified Data.ByteString                             as BS
import qualified Data.Map.Strict                             as Map
import qualified Data.Monoid                                 as Monoid
import qualified Data.Semigroup                              as Semigroup
import           Data.Text                                   (Text)
import           Data.Text.Prettyprint.Doc                   (Pretty (..),
                                                              viaShow)
import           GHC.Generics                                (Generic)
import           Ledger
import           Ledger.Ada                                  (adaSymbol,
                                                              adaToken,
                                                              adaValueOf,
                                                              lovelaceValueOf)
import           Ledger.Constraints
import qualified Ledger.Constraints.OffChain                 as Constraints
import qualified Ledger.Typed.Scripts                        as Scripts
import           Ledger.Value                                as Value
import           Plutus.Abstract.ContractResponse            (ContractResponse (..))
import           Plutus.Contract                             hiding (when)
import           Plutus.Contracts.Currency                   as Currency
import qualified Plutus.Contracts.NftMarketplace.Endpoints  as Marketplace
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core   as Marketplace
import           Plutus.PAB.Effects.Contract                 (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin         (Builtin,
                                                              SomeBuiltin (..),
                                                              type (.\\))
import qualified Plutus.PAB.Effects.Contract.Builtin         as Builtin
import           Plutus.PAB.Monitoring.PABLogMsg             (PABMultiAgentMsg)
import           Plutus.PAB.Simulator                        (Simulation,
                                                              SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                        as Simulator
import           Plutus.PAB.Types                            (PABError (..))
import qualified Plutus.PAB.Webserver.Server                 as PAB.Server
import           Plutus.V1.Ledger.Crypto                     (getPubKeyHash,
                                                              pubKeyHash)
import           Prelude                                     hiding (init)
import           Wallet.Emulator.Types                       (Wallet (..),
                                                              walletPubKey)
import           Wallet.Types                                (ContractInstanceId)

ownerWallet :: Wallet
ownerWallet = Wallet 1

userWallets :: [Wallet]
userWallets = [Wallet i | i <- [2 .. 4]]

data ContractIDs = ContractIDs { cidUser :: Map.Map Wallet ContractInstanceId, cidInfo :: ContractInstanceId }

activateContracts :: Simulation (Builtin MarketplaceContracts) ContractIDs
activateContracts = do
    cidStart <- Simulator.activateContract ownerWallet MarketplaceStart
    _  <- Simulator.callEndpointOnInstance cidStart "start" ()
    mp <- flip Simulator.waitForState cidStart $ \json -> case (fromJSON json :: Result (ContractResponse Text Marketplace.OwnerContractState)) of
        Success (ContractSuccess (Marketplace.Started mp)) -> Just mp
        _                                           -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Marketplace instance created: " ++ show mp

    -- cidInfo <- Simulator.activateContract ownerWallet $ MarketplaceInfo mp

    -- cidUser <- fmap Map.fromList $ forM userWallets $ \w -> do
    --     cid <- Simulator.activateContract w $ MarketplaceUser mp
    --     Simulator.logString @(Builtin MarketplaceContracts) $ "Marketplace user contract started for " ++ show w
    --     return (w, cid)

    pure $ ContractIDs Map.empty cidStart

runNftMarketplace :: IO ()
runNftMarketplace = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin MarketplaceContracts) "Starting Marketplace PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug
    ContractIDs {..} <- activateContracts
    let userCid = cidUser Map.! Wallet 2
        sender = pubKeyHash . walletPubKey $ Wallet 2

    _ <- liftIO getLine
    shutdown

data MarketplaceContracts =
    MarketplaceStart
    -- | MarketplaceInfo Marketplace.Marketplace
    -- | MarketplaceUser Marketplace.Marketplace
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty MarketplaceContracts where
    pretty = viaShow

handleMarketplaceContract ::
    ( Member (Error PABError) effs
    , Member (LogMsg (PABMultiAgentMsg (Builtin MarketplaceContracts))) effs
    )
    => ContractEffect (Builtin MarketplaceContracts)
    ~> Eff effs
handleMarketplaceContract = Builtin.handleBuiltin getSchema getContract where
  getSchema = \case
    -- MarketplaceUser _          -> Builtin.endpointsToSchemas @Marketplace.MarketplaceUserSchema
    -- MarketplaceInfo _          -> Builtin.endpointsToSchemas @Marketplace.MarketplaceInfoSchema
    MarketplaceStart           -> Builtin.endpointsToSchemas @Marketplace.MarketplaceOwnerSchema
    -- DistributeFunds _ _ -> Builtin.endpointsToSchemas @Empty
    -- CreateOracles _     -> Builtin.endpointsToSchemas @Empty
  getContract = \case
    -- MarketplaceInfo marketplace       -> SomeBuiltin $ Marketplace.infoEndpoints marketplace
    -- MarketplaceUser marketplace       -> SomeBuiltin $ Marketplace.userEndpoints marketplace
    MarketplaceStart           -> SomeBuiltin Marketplace.ownerEndpoints
    -- DistributeFunds wallets assets -> SomeBuiltin $ distributeFunds wallets assets
    -- CreateOracles assets -> SomeBuiltin $ createOracles assets

handlers :: SimulatorEffectHandlers (Builtin MarketplaceContracts)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin MarketplaceContracts) []
    $ interpret handleMarketplaceContract

oneAdaInLovelace :: Integer
oneAdaInLovelace = 1000000
