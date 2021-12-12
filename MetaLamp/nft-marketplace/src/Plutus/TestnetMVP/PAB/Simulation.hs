{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Plutus.TestnetMVP.PAB.Simulation where

import           Control.Monad                                  (forM, forM_,
                                                                 void, when)
import           Control.Monad.Freer                            (Eff, Member,
                                                                 interpret,
                                                                 type (~>))
import           Control.Monad.Freer.Error                      (Error)
import           Control.Monad.Freer.Extras.Log                 (LogMsg)
import           Control.Monad.IO.Class                         (MonadIO (..))
import qualified Data.Aeson                                     as J
import           Data.Default                                   (Default (def))
import qualified Data.Map.Strict                                as Map
import           Data.Monoid                                    (Last (..))
import qualified Data.Monoid                                    as Monoid
import qualified Data.OpenApi.Schema                            as OpenApi
import           Data.Proxy                                     (Proxy (..))
import qualified Data.Semigroup                                 as Semigroup
import           Data.Text                                      (Text)
import           Data.Text.Prettyprint.Doc                      (Pretty (..),
                                                                 viaShow)
import qualified Data.Time.Clock                                as Time
import           Ext.Plutus.Ledger.Time                         (Seconds (..),
                                                                 addToBeginningOfTime,
                                                                 convertUtcToPOSIX)
import qualified Ext.Plutus.PAB.Webserver.Server                as Ext.Plutus.PAB
import           GHC.Generics                                   (Generic)
import           Ledger
import           Ledger.Ada                                     (adaSymbol,
                                                                 adaToken,
                                                                 adaValueOf,
                                                                 lovelaceValueOf)
import qualified Ledger.Ada                                     as Ada
import           Ledger.Constraints
import qualified Ledger.Constraints.OffChain                    as Constraints
import           Ledger.TimeSlot                                (SlotConfig (..))
import qualified Ledger.Typed.Scripts                           as Scripts
import           Ledger.Value                                   as Value
import           Network.HTTP.Client                            (defaultManagerSettings,
                                                                 newManager)
import           Playground.Types                               (SimulatorWallet (..),
                                                                 adaCurrency)
import           Plutus.Abstract.ContractResponse               (ContractResponse,
                                                                 ContractState (..))
import           Plutus.Abstract.RemoteData                     (RemoteData (..))
import           Plutus.Contract                                hiding (when)
import qualified Plutus.TestnetMVP.OffChain.Endpoints      as Marketplace
import qualified Plutus.TestnetMVP.OffChain.Owner as Owner
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core   as Marketplace
import qualified Plutus.Contracts.Services.Sale                 as Sale
import           Plutus.PAB.Effects.Contract                    (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin            (Builtin,
                                                                 SomeBuiltin (..),
                                                                 type (.\\))
import qualified Plutus.PAB.Effects.Contract.Builtin            as Builtin
import           Plutus.TestnetMVP.PAB.MarketplaceContracts                (MarketplaceContracts (..))
import           Plutus.PAB.Monitoring.PABLogMsg                (PABMultiAgentMsg)
import           Plutus.PAB.Simulator                           (Simulation,
                                                                 SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                           as Simulator
import           Plutus.PAB.Types                               (PABError (..))
import qualified Plutus.PAB.Types                               as PAB
import qualified Plutus.PAB.Webserver.Server                    as PAB
import           Plutus.V1.Ledger.Time                          (DiffMilliSeconds (..),
                                                                 POSIXTime (..),
                                                                 fromMilliSeconds)
import           Prelude                                        hiding (init)
import           Servant.Client                                 (BaseUrl (..),
                                                                 Scheme (..),
                                                                 mkClientEnv)
import           Wallet.Emulator.Types                          (WalletNumber (..),
                                                                 walletPubKeyHash)
import           Wallet.Emulator.Wallet                         (Wallet (..),
                                                                 fromWalletNumber)
import           Wallet.Types                                   (ContractInstanceId)
import           Ledger.Index           (minAdaTxOut)
import Plutus.Contracts.NftMarketplace.OffChain.Serialization (deserializeByteString)

ownerWallet :: Wallet
ownerWallet = fromWalletNumber $ WalletNumber 1

wallet2 :: Wallet
wallet2 = fromWalletNumber $ WalletNumber 2

wallet3 :: Wallet
wallet3 = fromWalletNumber $ WalletNumber 3

userWallets :: [Wallet]
userWallets = fromWalletNumber <$> [WalletNumber i | i <- [2 .. 4]]

startMarketplaceParams :: Owner.StartMarketplaceParams
startMarketplaceParams = Owner.StartMarketplaceParams {
    marketplaceName = "Metalamp nft marketplace",
    creationFee = Ada.getLovelace minAdaTxOut,
    saleFee = (7, 2)
}

initialLotPrice :: Value.Value
initialLotPrice = lovelaceValueOf 100000000 -- 100 ADA

data ContractIDs = ContractIDs { cidUser :: Map.Map Wallet ContractInstanceId, cidInfo :: ContractInstanceId }

activateContracts :: Simulation (Builtin MarketplaceContracts) ContractIDs
activateContracts = do
    cidStart <- Simulator.activateContract ownerWallet MarketplaceStart
    _  <- Simulator.callEndpointOnInstance cidStart "start" startMarketplaceParams
    mp <- flip Simulator.waitForState cidStart $ \json -> case (J.fromJSON json :: J.Result (ContractResponse String Text Marketplace.OwnerContractState)) of
        J.Success (Last (Just (ContractState _ (Success (Marketplace.Started mp))))) -> Just mp
        _                                            -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Marketplace instance created: " ++ show mp

    cidInfo <- Simulator.activateContract ownerWallet $ MarketplaceInfo mp

    users <- fmap Map.fromList $ forM userWallets $ \w -> do
        cid <- Simulator.activateContract w $ MarketplaceUser mp
        Simulator.logString @(Builtin MarketplaceContracts) $ "Marketplace user contract started for " ++ show w
        return (w, cid)

    pure $ ContractIDs users cidInfo

runNftMarketplace :: IO ()
runNftMarketplace =
    void $ Simulator.runSimulationWith (handlers def) $ do
    Simulator.logString @(Builtin MarketplaceContracts) "Starting Marketplace PAB webserver on port 9080. Press enter to exit."
    shutdown <- PAB.startServerDebug
    ContractIDs {..} <- activateContracts
    let userCid = cidUser Map.! wallet2
        sender = walletPubKeyHash $ wallet2
    let catTokenIpfsCid = "QmPeoJnaDttpFrSySYBY3reRFCzL3qv4Uiqz376EBv9W16"
    let photoTokenIpfsCid = "QmeSFBsEZ7XtK7yv5CQ79tqFnH9V2jhFhSSq1LV5W3kuiB"

    _ <- Simulator.callEndpointOnInstance cidInfo "marketplaceSettings" ()
    v <- flip Simulator.waitForState cidInfo $ \json -> case (J.fromJSON json :: J.Result (ContractResponse String Text Marketplace.InfoContractState)) of
            J.Success (Last (Just (ContractState _ (Success (Marketplace.MarketplaceSettings v))))) -> Just v
            _                                           -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "MarketplaceSettings: " <> show v

    _  <-
        Simulator.callEndpointOnInstance userCid "createNft" $
            Marketplace.CreateNftParams {
                        cnpIpfsCid        = catTokenIpfsCid,
                        cnpNftName        = "Cat token",
                        cnpNftDescription = "A picture of a cat on a pogo stick",
                        cnpNftCategory = ["GIFs"],
                        cnpRevealIssuer   = False
                    }
    flip Simulator.waitForState userCid $ \json -> case (J.fromJSON json :: J.Result (ContractResponse String Text Marketplace.UserContractState)) of
        J.Success (Last (Just (ContractState _ (Success Marketplace.NftCreated)))) -> Just ()
        _                                          -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Successful createNft"

    -- _  <-
    --     Simulator.callEndpointOnInstance userCid "openSale" $
    --         Marketplace.OpenSaleParams {
    --                 ospItemId   = Marketplace.UserNftId catTokenIpfsCid,
    --                 ospSalePrice = 44*oneAdaInLovelace
    --             }
    -- sale <- flip Simulator.waitForState userCid $ \json -> case (J.fromJSON json :: J.Result (ContractResponse String Text Marketplace.UserContractState)) of
    --     J.Success (Last (Just (ContractState _ (Success Marketplace.OpenedSale)))) -> Just ()
    --     _                                          -> Nothing
    -- Simulator.logString @(Builtin MarketplaceContracts) $ "Successful openSale"

    -- let buyerCid = cidUser Map.! wallet3
    --     buyer = walletPubKeyHash wallet3

    -- _  <-
    --     Simulator.callEndpointOnInstance buyerCid "buyItem" Marketplace.CloseLotParams {
    --                                                             clpItemId   = Marketplace.UserNftId catTokenIpfsCid
    --                                                         }
    -- _ <- flip Simulator.waitForState buyerCid $ \json -> case (J.fromJSON json :: J.Result (ContractResponse String Text Marketplace.UserContractState)) of
    --     J.Success (Last (Just (ContractState _ (Success Marketplace.NftBought)))) -> Just ()
    --     _                                         -> Nothing
    -- Simulator.logString @(Builtin MarketplaceContracts) $ "Successful buyItem"

    -- _  <-
    --     Simulator.callEndpointOnInstance userCid "createNft" $
    --         Marketplace.CreateNftParams {
    --                     cnpIpfsCid        = photoTokenIpfsCid,
    --                     cnpNftName        = "Photo token",
    --                     cnpNftDescription = "A picture of a sunset",
    --                     cnpNftCategory = ["Photos"],
    --                     cnpRevealIssuer   = True
    --                 }
    -- flip Simulator.waitForState userCid $ \json -> case (J.fromJSON json :: J.Result (ContractResponse String Text Marketplace.UserContractState)) of
    --     J.Success (Last (Just (ContractState _ (Success Marketplace.NftCreated)))) -> Just ()
    --     _                                          -> Nothing
    -- Simulator.logString @(Builtin MarketplaceContracts) $ "Successful createNft"

    -- _ <- Simulator.waitNSlots 10

    -- _  <-
    --     Simulator.callEndpointOnInstance userCid "openSale" $
    --         Marketplace.OpenSaleParams {
    --                 ospItemId   = Marketplace.UserNftId photoTokenIpfsCid,
    --                 ospSalePrice = 12*oneAdaInLovelace
    --             }
    -- sale <- flip Simulator.waitForState userCid $ \json -> case (J.fromJSON json :: J.Result (ContractResponse String Text Marketplace.UserContractState)) of
    --     J.Success (Last (Just (ContractState _ (Success Marketplace.OpenedSale)))) -> Just ()
    --     _                                          -> Nothing
    -- Simulator.logString @(Builtin MarketplaceContracts) $ "Successful openSale"

    -- _ <- Simulator.waitNSlots 10

    -- _  <-
    --     Simulator.callEndpointOnInstance userCid "closeSale"
    --         Marketplace.CloseLotParams {
    --                 clpItemId   = Marketplace.UserNftId photoTokenIpfsCid
    --             }
    -- sale <- flip Simulator.waitForState userCid $ \json -> case (J.fromJSON json :: J.Result (ContractResponse String Text Marketplace.UserContractState)) of
    --     J.Success (Last (Just (ContractState _ (Success Marketplace.ClosedSale)))) -> Just ()
    --     _                                          -> Nothing
    -- Simulator.logString @(Builtin MarketplaceContracts) $ "Successful closeSale"

    -- _ <- Simulator.waitNSlots 10
    
    -- _ <- Simulator.callEndpointOnInstance cidInfo "fundsAt" buyer
    -- v <- flip Simulator.waitForState cidInfo $ \json -> case (J.fromJSON json :: J.Result (ContractResponse String Text Marketplace.InfoContractState)) of
    --         J.Success (Last (Just (ContractState _ (Success (Marketplace.FundsAt v))))) -> Just v
    --         _                                           -> Nothing
    -- Simulator.logString @(Builtin MarketplaceContracts) $ "Final buyer funds: " <> show v

    _ <- Simulator.callEndpointOnInstance cidInfo "marketplaceStore" ()
    marketplaceStore <- flip Simulator.waitForState cidInfo $ \json -> case (J.fromJSON json :: J.Result (ContractResponse String Text Marketplace.InfoContractState)) of
            J.Success (Last (Just (ContractState _ (Success (Marketplace.MarketplaceStore marketplaceStore))))) -> Just marketplaceStore
            _                                                  -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Final marketplaceStore: " <> show marketplaceStore

    _ <- Simulator.callEndpointOnInstance cidInfo "marketplaceFunds" ()
    v <- flip Simulator.waitForState cidInfo $ \json -> case (J.fromJSON json :: J.Result (ContractResponse String Text Marketplace.InfoContractState)) of
            J.Success (Last (Just (ContractState _ (Success (Marketplace.MarketplaceFunds v))))) -> Just v
            _                                                    -> Nothing
    Simulator.logString @(Builtin MarketplaceContracts) $ "Final marketplace funds: " <> show v

    -- _ <- Simulator.callEndpointOnInstance cidInfo "fundsAt" sender
    -- v <- flip Simulator.waitForState cidInfo $ \json -> case (J.fromJSON json :: J.Result (ContractResponse String Text Marketplace.InfoContractState)) of
    --         J.Success (Last (Just (ContractState _ (Success (Marketplace.FundsAt v))))) -> Just v
    --         _                                           -> Nothing
    -- Simulator.logString @(Builtin MarketplaceContracts) $ "Final user funds: " <> show v

    _ <- liftIO getLine
    shutdown

slotConfiguration :: POSIXTime -> SlotConfig
slotConfiguration beginningOfTime = SlotConfig
        { scSlotLength   = 1000
        , scSlotZeroTime = beginningOfTime
        }

handlers :: SlotConfig -> SimulatorEffectHandlers (Builtin MarketplaceContracts)
handlers slotConfig =
    Simulator.mkSimulatorHandlers def slotConfig
    $ interpret (Builtin.contractHandler (Builtin.handleBuiltin @MarketplaceContracts))

oneAdaInLovelace :: Integer
oneAdaInLovelace = 1000000
