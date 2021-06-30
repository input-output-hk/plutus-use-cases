{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExistentialQuantification #-}
module Plutus.Backend.Endpoints (activate, deposit) where
import           Control.Monad.IO.Class         (liftIO)
import qualified Data.Map.Strict                as Map
import qualified Data.Monoid                    as Monoid
import qualified Plutus.Backend.ContractStorage as ContractStorage
import           Plutus.Backend.Types
import           Plutus.ContractStorage
import           Plutus.Contracts.Endpoints     (ContractResponse (..))
import qualified Plutus.Contracts.Endpoints     as Aave
import qualified Plutus.PAB.Simulation          as Simulation
import qualified Plutus.PAB.Simulator           as Simulator
import           Plutus.V1.Ledger.Crypto        (getPubKeyHash, pubKeyHash)
import           Prelude                        hiding (init)
import           Servant
import           Servant.Server
import           Wallet.Emulator.Types          (Wallet (..), walletPubKey)
import           Plutus.PAB.Effects.Contract.Builtin (Builtin)

deposit :: WithContractStorage => DepositRequest -> IO OperationStatus
deposit depositRequest@DepositRequest{..} =  do
    mbContractIDs <- ContractStorage.getContractId wallet "activate"
    case mbContractIDs of
        Nothing -> return . FailOperation $ "Can't find contractId for wallet: " <> show wallet
        Just contractIDs -> makeDeposit depositRequest contractIDs

activate :: forall t . WithContractStorage => String -> Simulator.Simulation (Builtin Simulation.AaveContracts) ()
activate endpoint = do
    contractIDs <- Simulation.activateContracts
    liftIO $ ContractStorage.saveContractIds contractIDs endpoint

makeDeposit :: 
    WithContractStorage => 
    DepositRequest -> 
    Simulation.ContractIDs -> 
    IO OperationStatus
makeDeposit DepositRequest{..} contractIDs = do
    operationResult <- Simulator.runSimulationWith Simulation.handlers $ do
        let sender = pubKeyHash $ walletPubKey wallet
        let depositParams = Aave.DepositParams {
            Aave.dpAsset = head Simulation.testAssets, 
            Aave.dpOnBehalfOf = sender, Aave.dpAmount = amount
            }
        Simulation.depositSimulation contractIDs depositParams wallet
    case operationResult of
        Left err -> return $ FailOperation (show err)
        _        -> return SuccessOperation
