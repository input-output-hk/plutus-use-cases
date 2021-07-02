{-# LANGUAGE TypeApplications #-}

module Plutus.PAB.ContractsActivation where
import           Data.Aeson                          (FromJSON, Result (..),
                                                      ToJSON, encode, fromJSON)
import qualified Data.Monoid                         as Monoid
import           Data.Text                           (Text)
import qualified Plutus.Contracts.Core               as Aave
import           Plutus.Contracts.Endpoints          (ContractResponse (..))
import qualified Plutus.Contracts.Endpoints          as Aave
import qualified Plutus.Contracts.Oracle             as Oracle
import           Plutus.PAB.Effects.Contract.Builtin (Builtin)
import           Plutus.PAB.Simulation
import           Plutus.PAB.Simulator                (Simulation)
import qualified Plutus.PAB.Simulator                as Simulator
import           Wallet.Emulator.Types               (Wallet (..), walletPubKey)
import           Wallet.Types                        (ContractInstanceId)

activateFunds :: Wallet -> [Wallet] -> Simulation (Builtin AaveContracts) ContractInstanceId
activateFunds wallet userWallets= do
    cidFunds <- Simulator.activateContract wallet $ DistributeFunds userWallets testAssets
    _        <- Simulator.waitUntilFinished cidFunds
    Simulator.logString @(Builtin AaveContracts) "Wallet initialization finished"
    return cidFunds

activateOracles :: Wallet -> Simulation (Builtin AaveContracts) (ContractInstanceId, [Oracle.Oracle])
activateOracles wallet = do
    cidOracles  <- Simulator.activateContract wallet $ CreateOracles testAssets
    oracles  <- flip Simulator.waitForState cidOracles $ \json -> case (fromJSON json :: Result (Monoid.Last [Oracle.Oracle])) of
                    Success (Monoid.Last (Just res)) -> Just res
                    _                                -> Nothing
    Simulator.logString @(Builtin AaveContracts) "Oracles initialization finished."
    return (cidOracles, oracles)

-- forge tokens for assets
startAaveInstance :: Wallet -> [Oracle.Oracle] -> Simulation (Builtin AaveContracts) (ContractInstanceId, Aave.Aave)
startAaveInstance wallet oracles = do
    cidStart <- Simulator.activateContract wallet AaveStart
    _  <- Simulator.callEndpointOnInstance cidStart "start" $ fmap (\o -> Aave.CreateParams (Oracle.oAsset o) o) oracles
    aa <- flip Simulator.waitForState cidStart $ \json -> case (fromJSON json :: Result (Monoid.Last (ContractResponse Text Aave.OwnerContractState))) of
        Success (Monoid.Last (Just (ContractSuccess (Aave.Started aa)))) -> Just aa
        _                                       -> Nothing
    Simulator.logString @(Builtin AaveContracts) $ "Aave instance created: " ++ show aa
    return (cidStart, aa)

activateAaveInfo :: Wallet -> Aave.Aave -> Simulation (Builtin AaveContracts) ContractInstanceId
activateAaveInfo wallet aa = Simulator.activateContract wallet $ AaveInfo aa

activateAaveUser :: Wallet -> ContractInstanceId -> Aave.Aave -> Simulation (Builtin AaveContracts) (Wallet, ContractInstanceId)
activateAaveUser wallet cidFunds aave = do
    cId <- Simulator.activateContract wallet $ AaveUser aave
    _        <- Simulator.waitUntilFinished cidFunds
    Simulator.logString @(Builtin AaveContracts) "Contract initialization finished."
    return (ownerWallet, cId)

