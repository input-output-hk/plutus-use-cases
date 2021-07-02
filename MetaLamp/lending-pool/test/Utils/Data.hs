module Utils.Data where

import           Data.Function                    ((&))
import           Plutus.Abstract.ContractResponse (ContractResponse (..))
import           Plutus.V1.Ledger.Crypto          (PubKeyHash, pubKeyHash)
import qualified PlutusTx.AssocMap                as AssocMap
import qualified PlutusTx.Prelude                 as PlutusTx
import           Wallet.Emulator.Wallet           (Wallet, walletPubKey)

allSatisfy :: [a -> Bool] -> a -> Bool
allSatisfy fs a = and . fmap (a &) $ fs

one :: (a -> Bool) -> [a] -> Bool
one f = foldr reducer False
    where
        reducer cur acc = if acc then not . f $ cur else f cur

modifyAt :: PlutusTx.Eq k => (v -> v) -> k -> AssocMap.Map k v -> AssocMap.Map k v
modifyAt f k m = maybe m (\v -> AssocMap.insert k (f v) m) (AssocMap.lookup k m)

isLastError :: ContractResponse e a -> Bool
isLastError (ContractError _) = True
isLastError _                 = False

getPubKey :: Wallet -> PubKeyHash
getPubKey = pubKeyHash . walletPubKey
