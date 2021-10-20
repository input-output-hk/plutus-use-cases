{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Utils.Trace where


import           Control.Lens               ((^?))
import           Data.Maybe                 (isJust)
import           GHC.TypeLits               (KnownSymbol)
import           Plutus.Abstract.RemoteData (RemoteData (..))
import qualified Plutus.Abstract.RemoteData as RD
import qualified Plutus.Contract            as C
import           Plutus.Contract.Test       (TracePredicate, assertAccumState)
import qualified Plutus.Trace.Emulator      as Trace

assertRDError :: forall contract e r s err a proxy l. (Show r, Show e, C.IsContract contract, KnownSymbol l) =>
    proxy l ->
    contract (RemoteData e r) s err a
    -> Trace.ContractInstanceTag
    -> TracePredicate
assertRDError _p c tag = assertAccumState c tag RD.isFailure "Expected contract error but there was none"
