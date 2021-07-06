module Main
     ( main
     ) where

import           Control.Monad             (void)
import           Data.Bifunctor            (first)
import qualified Data.Aeson as JSON
import Data.Maybe
import qualified Data.Text                 as T
import           Plutus.PAB.ContractCLI    (commandLineApp)
import           Plutus.Contracts.LendingPool.OnChain.Core (aave)
import           Plutus.Contracts.LendingPool.OnChain.Core.Validator (Aave (..))
import qualified Plutus.Contracts.LendingPool.OffChain.User  as Aave
import           Plutus.V1.Ledger.Value (CurrencySymbol)
import qualified PlutusTx.Builtins as Builtins
import Plutus.PAB.Simulation

main :: IO ()
main = do
     aa <- JSON.decodeFileStrict "aa.aave"
     commandLineApp $ first (T.pack . show) $ void $ Aave.userEndpoints $ fromJust aa