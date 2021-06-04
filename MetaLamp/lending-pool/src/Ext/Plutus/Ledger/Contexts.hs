{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Ext.Plutus.Ledger.Contexts where

import           Ledger           (DatumHash, Value)
import           PlutusTx.Prelude (Eq ((==)), Maybe, find, fst, (<$>))

{-# INLINABLE findDatumHashByValue #-}
-- | Find the hash of a datum, if it is part of the pending transaction's
--   outputs
findDatumHashByValue :: Value -> [(DatumHash, Value)] -> Maybe DatumHash
findDatumHashByValue val outs = fst <$> find f outs
  where
    f (_, val') = val' == val
