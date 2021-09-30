{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Utils.Trace where


import           Control.Lens                     ((^?))
import           Data.Maybe                       (isJust)
import           GHC.TypeLits                     (KnownSymbol)
import           Plutus.Abstract.ContractResponse (ContractResponse,
                                                   getEndpointStatus)
import qualified Plutus.Abstract.RemoteData       as RD
import qualified Plutus.Contract                  as C
import           Plutus.Contract.Test             (TracePredicate,
                                                   assertAccumState)
import qualified Plutus.Trace.Emulator            as Trace

assertCrError :: forall contract e r s err a proxy l. (Show r, Show e, C.IsContract contract, KnownSymbol l) =>
    proxy l ->
    contract (ContractResponse e r) s err a
    -> Trace.ContractInstanceTag
    -> TracePredicate
assertCrError p c tag = assertAccumState c tag isError "Expected contract error but there was none"
    where
        isError :: ContractResponse e r -> Bool
        isError = RD.isFailure . getEndpointStatus p


