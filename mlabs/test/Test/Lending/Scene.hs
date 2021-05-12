-- | Set of balances for tests
module Test.Lending.Scene(
    Scene(..)
  , owns
  , appOwns
  , checkScene
  , coinDiff
) where

import Data.Map (Map)
import Plutus.V1.Ledger.Value (Value)
import Plutus.Contract.Test hiding (tx)
import Mlabs.Lending.Logic.Types (Coin)
import qualified Plutus.V1.Ledger.Value as Value
import qualified Data.Map as M

import Test.Utils

-- | Scene is users with balances and value that is owned by application script
data Scene = Scene
  { scene'users   :: Map Wallet Value   -- ^ user balances
  , scene'app     :: Value              -- ^ application script balance
  }

instance Semigroup Scene where
  Scene us1 e1 <> Scene us2 e2 = Scene (M.unionWith (<>) us1 us2) (e1 <> e2)

instance Monoid Scene where
  mempty = Scene mempty mempty

owns :: Wallet -> [(Coin, Integer)] -> Scene
owns wal ds = Scene { scene'users = M.singleton wal (coinDiff ds), scene'app = mempty }

appOwns :: [(Coin, Integer)] -> Scene
appOwns v = Scene { scene'users = mempty, scene'app = coinDiff v }

checkScene :: Scene -> TracePredicate
checkScene Scene{..} =
  (concatPredicates $ fmap (uncurry walletFundsChange) $ M.toList scene'users)
  .&&. assertNoFailedTransactions

coinDiff :: [(Coin, Integer)] -> Value
coinDiff = foldMap (uncurry Value.assetClassValue)

