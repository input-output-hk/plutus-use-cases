{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module Plutus.Backend.ContractStorage where
import           Control.Monad                       (mapM, void)
import           Control.Monad.IO.Class              (liftIO)
import qualified Data.Map.Strict                     as Map
import           Data.Maybe                          (fromMaybe)
import qualified Plutus.ContractStorage              as Storage
import           Plutus.PAB.Effects.Contract.Builtin (Builtin)
import qualified Plutus.PAB.Simulation               as Simulation
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Wallet.Emulator.Types               (Wallet (..))

saveContractIds :: Storage.WithContractStorage => Simulation.ContractIDs -> Storage.Endpoint -> IO ()
saveContractIds Simulation.ContractIDs {..} endpoint = do
      let cIds = Map.toList cidUser
      mapM (\(wallet, ciId) -> Storage.saveContractId wallet endpoint ciId) cIds
      return ()

getContractId :: Storage.WithContractStorage => Wallet -> Storage.Endpoint -> IO (Maybe Simulation.ContractIDs)
getContractId wallet endpoint = do
    contractId <- Storage.getContractId wallet endpoint
    pure $ maybe
        Nothing
        (\cId -> Just $ Simulation.ContractIDs {cidUser = Map.singleton wallet cId, cidInfo = cId})
        contractId
