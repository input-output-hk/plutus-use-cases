module Main
     ( main
     ) where

import           Control.Monad             (void)
import           Data.Bifunctor            (first)
import qualified Data.Aeson as JSON
import qualified Data.Text                 as T
import           Plutus.PAB.ContractCLI    (commandLineApp)
import           Plutus.Contracts.LendingPool.OnChain.Core (aave)
import           Plutus.Contracts.LendingPool.OnChain.Core.Validator (Aave (..))
import qualified Plutus.Contracts.LendingPool.OffChain.Owner  as Aave
import           Plutus.V1.Ledger.Value (CurrencySymbol)
import qualified PlutusTx.Builtins as Builtins
import Plutus.PAB.Simulation

main :: IO ()
main = commandLineApp $ first (T.pack . show) $ void $ Aave.ownerEndpoints