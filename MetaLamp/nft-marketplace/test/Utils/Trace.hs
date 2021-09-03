{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Utils.Trace where


import           Control.Lens                     ((^?))
import           Data.Maybe                       (isJust)
import           Plutus.Abstract.ContractResponse (AsContractResponse (_CrError),
                                                   ContractResponse)
import qualified Plutus.Contract                  as C
import           Plutus.Contract.Test             (TracePredicate,
                                                   assertAccumState)
import qualified Plutus.Trace.Emulator            as Trace

assertCrError :: forall contract e r s err a. (Show r, Show e, C.IsContract contract) =>
    contract (ContractResponse e r) s err a
    -> Trace.ContractInstanceTag
    -> TracePredicate
assertCrError c tag = assertAccumState c tag (isJust . (^? _CrError)) "Expected contract error but there was none"
