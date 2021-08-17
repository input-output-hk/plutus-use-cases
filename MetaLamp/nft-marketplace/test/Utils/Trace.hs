{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Utils.Trace where

import qualified Control.Foldl                    as L
import           Control.Monad                    (unless)
import           Control.Monad.Freer.Error        (throwError)
import           Control.Monad.Freer.Writer       (tell)
import qualified Data.Aeson                       as JSON

import qualified Data.Map                         as Map
import           Data.Maybe                       (mapMaybe)
import           Data.String                      (fromString)
import           Data.Text.Prettyprint.Doc        (Doc)
import           Data.Void                        (Void)
import           Ledger                           (Address)
import qualified Ledger
import           Ledger.AddressMap                (UtxoMap)
import           Plutus.Abstract.ContractResponse (ContractResponse (..))
import           Plutus.Contract.Test             (TracePredicate)
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
        ContractSuccess s -> maybe (throwError . GenericError $ "Unexpected state: " <> show s) pure (pick s)
        ContractError e -> throwError . GenericError . show $ e
        ContractPending -> Trace.waitNSlots 1 >> waitForState pick userHandle
