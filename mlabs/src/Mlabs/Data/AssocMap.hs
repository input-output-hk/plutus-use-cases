-- | Missing plutus functions for AssocMap's
module Mlabs.Data.AssocMap(
  filter
)  where

import PlutusTx.Prelude (Bool, (.), ($), snd)

import qualified PlutusTx.Prelude as Plutus (filter)
import PlutusTx.AssocMap (Map)
import qualified PlutusTx.AssocMap as M

filter :: (v -> Bool) -> Map k v -> Map k v
filter f m = M.fromList $ Plutus.filter (f . snd) $ M.toList m

