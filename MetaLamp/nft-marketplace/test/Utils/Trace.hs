{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}

module Utils.Trace where

import qualified Control.Foldl                    as L
import           Control.Monad                    (unless)
import           Control.Monad.Freer.Error        (throwError)
import           Control.Monad.Freer.Writer       (tell)
import qualified Data.Aeson                       as JSON

import           Control.Lens                     (_2, (&), (.~), (^.), (^?))
import qualified Control.Lens                     as Lens
import qualified Data.Map                         as Map
import           Data.Maybe                       (isJust, mapMaybe)
import           Data.String                      (fromString)
import           Data.Text.Prettyprint.Doc        (Doc)
import           Data.Void                        (Void)
import           Ledger                           (Address)
import qualified Ledger
import           Ledger.AddressMap                (UtxoMap)
import           Plutus.Abstract.ContractResponse
import qualified Plutus.Contract                  as C
import           Plutus.Contract.Test             (TracePredicate,
                                                   assertAccumState)
import qualified Plutus.Trace.Emulator            as Trace
import           Plutus.Trace.Emulator.Types      (EmulatorRuntimeError (..))
import           PlutusTx                         (IsData, fromData)
import qualified Wallet.Emulator.Folds            as Folds
import           Wallet.Emulator.MultiAgent       (EmulatorEvent)

waitForState ::
    (Show a
    , Show e
    , Trace.ContractConstraints s
    , JSON.FromJSON e
    , JSON.FromJSON a
    , JSON.ToJSON e
    , JSON.ToJSON a
    , JSON.FromJSON e'
    )
    => (a -> Maybe b) ->
    Trace.ContractHandle (ContractResponse e a) s e' ->
    Trace.EmulatorTrace b
waitForState pick userHandle = do
    res <- Trace.observableState userHandle
    case res of
        CrSuccess s -> maybe (throwError . GenericError $ "Unexpected state: " <> show s) pure (pick s)
        CrError e -> throwError . GenericError . show $ e
        CrPending -> Trace.waitNSlots 1 >> waitForState pick userHandle

assertCrError :: forall e r s err a. (Show r, Show e) =>
    C.Contract (ContractResponse e r) s err a
    -> Trace.ContractInstanceTag
    -> TracePredicate
assertCrError c tag = assertAccumState c tag (isJust . (^? _CrError)) "Expected contract error but there was none"
