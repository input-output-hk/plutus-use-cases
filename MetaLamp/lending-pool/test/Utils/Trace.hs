{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Utils.Trace where

import qualified Control.Foldl               as L
import           Control.Monad               (unless)
import           Control.Monad.Freer.Error   (throwError)
import           Control.Monad.Freer.Writer  (tell)
import qualified Data.Aeson                  as JSON

import qualified Data.Map                    as Map
import           Data.Maybe                  (mapMaybe)
import           Data.Monoid                 (Last (..))
import           Data.String                 (fromString)
import           Data.Text.Prettyprint.Doc   (Doc)
import           Data.Void                   (Void)
import           Ledger                      (Address)
import qualified Ledger
import           Ledger.AddressMap           (UtxoMap)
import           Plutus.Contract             (HasBlockchainActions)
import           Plutus.Contract.Test        (TracePredicate)
import           Plutus.Contracts.Endpoints  (ContractResponse (..))
import qualified Plutus.Trace.Emulator       as Trace
import           Plutus.Trace.Emulator.Types (EmulatorRuntimeError (..))
import           PlutusTx                    (IsData, fromData)
import qualified Wallet.Emulator.Folds       as Folds
import           Wallet.Emulator.MultiAgent  (EmulatorEvent)

getState ::
    (Show a
    , Show e
    , HasBlockchainActions s
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
getState pick userHandle = do
    res <- Trace.observableState userHandle
    case res of
        ContractSuccess s -> maybe (throwError . GenericError $ "Unexpected state: " <> show s) pure (pick s)
        ContractError e -> throwError . GenericError . show $ e
        s -> throwError . JSONDecodingError $ "Unexpected state: " <> show s

utxoAtAddress :: Monad m => Address -> (UtxoMap -> m c)-> L.FoldM m EmulatorEvent c
utxoAtAddress address check = Folds.postMapM check (L.generalize $ Folds.utxoAtAddress address)

datumsAtAddress :: (IsData a, Show a) => Address -> ([a] -> Bool) -> TracePredicate
datumsAtAddress address check = utxoAtAddress address $ \utxo -> do
    let datums = getDatums utxo
        result = check datums
    unless result $ tell @(Doc Void) (fromString $ "Datum check failed: " <> show datums)
    pure result

getDatums :: IsData a => UtxoMap -> [a]
getDatums = mapMaybe findDatum . Map.elems

findDatum :: PlutusTx.IsData a => Ledger.TxOutTx -> Maybe a
findDatum o = do
    hash <- Ledger.txOutDatumHash $ Ledger.txOutTxOut o
    (Ledger.Datum e) <- Map.lookup hash $ Ledger.txData $ Ledger.txOutTxTx o
    PlutusTx.fromData e
