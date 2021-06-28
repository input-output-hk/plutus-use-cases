{-# LANGUAGE RecordWildCards #-}

module Plutus.Backend.Endpoints where
import Wallet.Emulator.Types (Wallet (..), walletPubKey)
import Plutus.PAB.Simulation 
import           Plutus.V1.Ledger.Crypto             (getPubKeyHash, pubKeyHash)
import Plutus.Backend.Types 
import Plutus.Backend.ContractStorage
import Servant
import qualified Data.Map.Strict                     as Map
import qualified Plutus.PAB.Simulator                as Simulator
import           Plutus.Contracts.Endpoints          (ContractResponse (..))
import qualified Plutus.Contracts.Endpoints          as Aave

import Data.Text
import qualified Data.Monoid                         as Monoid

deposit :: WithContractStorage => DepositRequest -> Handler OperationStatus
deposit DepositRequest {..} = return $ OperationStatus Deposit SuccessStatus

