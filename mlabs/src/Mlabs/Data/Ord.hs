-- | Missing plutus functions for Ord.
module Mlabs.Data.Ord(
  comparing
) where

import PlutusTx.Prelude

{-# INLINABLE comparing #-}
-- |
-- > comparing p x y = compare (p x) (p y)
--
-- Useful combinator for use in conjunction with the @xxxBy@ family
-- of functions from "Data.List", for example:
--
-- >   ... sortBy (comparing fst) ...
comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
comparing p x y = compare (p x) (p y)

