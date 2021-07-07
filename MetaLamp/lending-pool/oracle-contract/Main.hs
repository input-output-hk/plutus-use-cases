module Main
    ( main
    ) where

import           Control.Monad             (void)
import           Data.Bifunctor            (first)
import qualified Data.Text                 as T
import           Plutus.PAB.ContractCLI    (commandLineApp)
import           Plutus.Contracts.LendingPool.OnChain.Core (aave)
import           Plutus.Contracts.LendingPool.OnChain.Core.Validator (Aave (..))
import           Plutus.V1.Ledger.Value (CurrencySymbol)
import qualified PlutusTx.Builtins as Builtins
import Plutus.Contracts.Service.OracleContract (oracleEndpoints)

main :: IO ()
main = commandLineApp $ first (T.pack . show) $ void oracleEndpoints
