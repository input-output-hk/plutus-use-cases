{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Plutus.Contract.Wallet.Utils
where

import Data.Map ( elems )
import Plutus.Contract

import Data.Text ( Text )
import Ledger ( pubKeyAddress, TxOut (txOutValue), TxOutTx (txOutTxOut, txOutTxTx), txOutTxDatum, TxOutRef, Address, DatumHash, Datum(..), Tx (txData), txOutDatum)
import Ledger.Value ( Value, flattenValue )
import Data.Monoid
import Control.Monad (void)
import qualified Data.Aeson.Types as Types
import qualified Data.Map as Map
import Data.Aeson (toJSON)
import Ledger.AddressMap
import PlutusTx
import Playground.Contract ( TxOutRef )
import Data.Maybe ( isJust, fromJust, catMaybes, mapMaybe )
import Data.Functor ((<&>))
import Control.Lens (review)
import Plutus.Contract.Types
import Plutus.Contract.Constraints (MkTxError(TxOutRefNotFound, TxOutRefWrongType))


type   ParsedUtxo a =  (TxOutRef,TxOutTx, a)
-- Transform Utxo Map to list.
-- But include only those utxos that have expected Datum type. Ignore others.
flattenUtxosWithData ::   IsData a =>   UtxoMap  -> [ParsedUtxo a]
flattenUtxosWithData m= mapMaybe doTransform $ Map.toList m
  where
    doTransform (ref,txOutTx) =txOutTxData txOutTx <&> (ref,txOutTx,)

 -- Find All utxos  at address and return It's reference, original transacction and   resolved datum of utxo
 -- The utxos that don't have expected data type are ignored.
utxosWithDataAt ::    ( AsContractError e,IsData a) =>
               Address ->Contract w s e [ParsedUtxo a]
utxosWithDataAt address=do
    utxos<-utxoAt address
    pure  $ flattenUtxosWithData utxos

-- With Filter funciton f, return list containing reference, parent transaction 
-- and resolved. data of the utxo.
-- Utxos that don't have expected data type are ignored
filterUtxosWithDataAt ::    ( AsContractError e,IsData a) =>
               (TxOutRef-> TxOutTx -> Bool) -> Address ->Contract w s e [ParsedUtxo a]
filterUtxosWithDataAt f addr =do
    utxos<-utxoAt addr
    let responses =  Map.filterWithKey f  utxos
    pure $ flattenUtxosWithData responses

-- Given TxoutReferences, find thost at given address, and resolve the datum field to expected 
-- data type
resolveRefsWithDataAt:: (IsData  a,AsContractError e) => Address  ->[TxOutRef]  -> Contract w s e [ParsedUtxo a]
resolveRefsWithDataAt addr refs= do
    utxos <- utxoAt addr
    let doResolve x =( do
                tx <- Map.lookup x utxos
                d <- txOutTxData tx
                pure (x,tx,d)
          )
    pure $ mapMaybe doResolve  refs

--  resolve UtxoRefs and return them with datum. If the datum is not in expected type, throw error
resolveRefsWithDataAtWithError :: (IsData  a,AsContractError e) => Address  ->[TxOutRef]  -> Contract w s e [ParsedUtxo a]
resolveRefsWithDataAtWithError addr refs =do
  utxos <-utxoAt addr
  mapM  (resolveTxOutRefWithData utxos)  refs


-- Given TxOut Reference, Resolve it's transaction 
-- and the datum info expected data type
-- If utxo is not found or datum couldn't be transformed properly, It will throw error.
resolveRefWithDataAt:: (IsData  a,AsContractError e) => Address  ->TxOutRef  -> Contract w s e  (ParsedUtxo a)
resolveRefWithDataAt addr ref = utxoAt addr >>= flip resolveTxOutRefWithData ref

-- From a utxo reference, find out datum in it.
--
resolveTxOutRefWithData::(IsData a,AsContractError e) =>
  UtxoMap -> TxOutRef -> Contract  w s e (ParsedUtxo a)
resolveTxOutRefWithData  utxos ref=  case Map.lookup ref utxos of
    Just tx -> case txOutTxData tx <&> (ref, tx,) of
            Just v -> return v
            Nothing -> throwError  $ review _ConstraintResolutionError   $ TxOutRefWrongType ref
    _       -> throwError  $ review _ConstraintResolutionError   $ TxOutRefNotFound ref


txOutRefData :: (IsData a) => UtxoMap  -> TxOutRef -> Maybe a
txOutRefData  dataMap ref=do
    tx <-Map.lookup ref dataMap
    txOutTxData tx


-- Given TxOutTx, resolve Datum in the Utxo to expected type

txOutTxData :: (IsData a)=>TxOutTx -> Maybe a
txOutTxData o =mappedData (txOutTxOut o) $ \dh -> Map.lookup dh $ txData $ txOutTxTx o
    where
    mappedData :: IsData a => TxOut -> (DatumHash -> Maybe Datum) -> Maybe a
    mappedData o f = do
        dh      <- txOutDatum o
        d <- f dh
        fromData $ getDatum d


--------------
-------------- Utility Endpoints
--------------

-- get funds in this wallet
ownFunds ::  Contract w s Text Value
ownFunds = do
    pk    <- ownPubKey
    utxos <- utxoAt $ pubKeyAddress pk
    pure . mconcat . elems $ txOutValue . txOutTxOut <$> utxos

type UtilSchema=
  Endpoint "funds" String

utilEndpoints :: HasEndpoint "funds" String s => Contract [Types.Value] s Text ()
utilEndpoints= handleError (\e ->logError e) $ void fundsEp 

fundsEp :: HasEndpoint "funds" String s => Contract
  [Types.Value] s Text Types.Value
fundsEp= do
    endpoint @"funds"
    v<- ownFunds
    tell [ toJSON v]
-- let's hope that in future we can return the json string without having to tell
    return $ toJSON  v 

throwNoUtxo::AsContractError e =>Contract w s e a
throwNoUtxo=throwError  $ review _OtherError "No valid Utxo to consume"

otherError :: ( AsContractError e) =>Text -> Contract w s e a
otherError s = throwError  $ review _OtherError s