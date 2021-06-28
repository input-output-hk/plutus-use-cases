{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
module Plutus.Backend.Endpoints where
import qualified Data.Map.Strict                as Map
import qualified Data.Monoid                    as Monoid
import           Plutus.Backend.ContractStorage
import           Plutus.Backend.Types
import           Plutus.Contracts.Endpoints     (ContractResponse (..))
import qualified Plutus.Contracts.Endpoints     as Aave
import qualified Plutus.PAB.Simulation          as Simulation
import qualified Plutus.PAB.Simulator           as Simulator
import           Plutus.V1.Ledger.Crypto        (getPubKeyHash, pubKeyHash)
import           Prelude                        hiding (init)
import           Servant
import           Servant.Server
import           Wallet.Emulator.Types          (Wallet (..), walletPubKey)

deposit :: WithContractStorage => DepositRequest -> IO OperationStatus
deposit DepositRequest {..} = do
    result <-
        Simulator.runSimulationWith Simulation.handlers $ do
        contractIDs <- Simulation.activateContracts
        let sender = pubKeyHash $ walletPubKey wallet
        let depositParams = Aave.DepositParams {Aave.dpAsset = head Simulation.testAssets, Aave.dpOnBehalfOf = sender, Aave.dpAmount = amount}
        Simulation.depositSimulation contractIDs depositParams wallet
    case result of
        Left err -> return $ FailOperation (show err)
        _        -> return SuccessOperation



