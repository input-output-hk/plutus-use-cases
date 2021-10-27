{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Utils.Trace where


import           Control.Lens                     ((^?))
import           Data.Maybe                       (isJust)
import           Data.Monoid                      (Last (..))
import           GHC.TypeLits                     (KnownSymbol)
import           Plutus.Abstract.ContractResponse (ContractResponse,
                                                   ContractState (..))
import           Plutus.Abstract.RemoteData       (RemoteData (..))
import qualified Plutus.Abstract.RemoteData       as RD
import qualified Plutus.Contract                  as C
import           Plutus.Contract.Test             (TracePredicate,
                                                   assertAccumState)
import qualified Plutus.Trace.Emulator            as Trace

assertCrError :: forall contract e r s err a proxy l. (Show r, Show e, C.IsContract contract, KnownSymbol l) =>
    proxy l ->
    contract (ContractResponse String e r) s err a
    -> Trace.ContractInstanceTag
    -> TracePredicate
assertCrError _p c tag = assertAccumState c tag isError "Expected contract error but there was none"
    where
        isError :: ContractResponse String e r -> Bool
        isError (Last (Just (ContractState _ rd))) = RD.isFailure rd
