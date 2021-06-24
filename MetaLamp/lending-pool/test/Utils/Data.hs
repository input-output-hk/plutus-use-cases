module Utils.Data where

import           Data.Function     ((&))
import qualified PlutusTx.AssocMap as AssocMap
import qualified PlutusTx.Prelude  as PlutusTx
import           Data.Monoid                (Last (..))
import           Plutus.Contracts.Endpoints (ContractResponse (..))

allSatisfy :: [a -> Bool] -> a -> Bool
allSatisfy fs a = and . fmap (a &) $ fs

one :: (a -> Bool) -> [a] -> Bool
one f = foldr reducer False
    where
        reducer cur acc = if acc then not . f $ cur else f cur

modifyAt :: PlutusTx.Eq k => (v -> v) -> k -> AssocMap.Map k v -> AssocMap.Map k v
modifyAt f k m = maybe m (\v -> AssocMap.insert k (f v) m) (AssocMap.lookup k m)

isLastError :: Last (ContractResponse e a) -> Bool
isLastError (Last (Just (ContractError _))) = True
isLastError _                               = False
