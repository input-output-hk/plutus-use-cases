{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE TupleSections #-}
module Plutus.Contract.Blockchain.Utils
where
import PlutusTx.Prelude
import Ledger.Value as Value ( Value )
import           Ledger.Contexts          (ScriptContext (..), TxInfo (..))
import Ledger
    ( findDatum,
      findOwnInput,
      ownHash,
      valueLockedBy,
      ScriptContext(ScriptContext, scriptContextTxInfo),
      TxInInfo(TxInInfo, txInInfoResolved),
      TxInfo(TxInfo, txInfoInputs),
      TxOut(..),
      Value,
      Datum(getDatum),
      scriptHashAddress,
      Address, toValidatorHash )
import PlutusTx

--  The functions in this  module are not bounded to the marketplace use case.
--- These functions should probably be provided by the Plutus Library itself.


-- address of this validator
{-# INLINABLE ownAddress #-}
ownAddress :: ScriptContext -> Address
ownAddress ctx=scriptHashAddress (ownHash ctx)

-- all the utxos that are being redeemed from this contract in this transaction
{-# INLINABLE  ownInputs #-}
ownInputs:: ScriptContext -> [TxOut]
ownInputs ctx@ScriptContext{scriptContextTxInfo=TxInfo{txInfoInputs}}=
     filter (\x->txOutAddress x==ownAddress ctx) resolved
    where
    resolved=map (\x->txInInfoResolved x) txInfoInputs

--
--  Commented code below is invalid because ScriptCredential is not exported by Ledger.Address.
--  Better add this function to  the library

-- allowSingleScript:: ScriptContext  -> Bool
-- allowSingleScript ctx@ScriptContext{scriptContextTxInfo=TxInfo{txInfoInputs}} =
--     checkScript txInfoInputs
--   where
--     checkScript (TxInInfo _ (TxOut address _ _))= 
--       case addressCredential  address of
--         ScriptCredential vhash ->  traceIfFalse  @String "Reeming other Script utxo is Not allowed" (thisScriptHash == vhash)
--         _ -> True
--     thisScriptHash= ownHash ctx

-- get List of valid parsed datums to the script in this transaction
{-# INLINABLE ownInputDatums #-}
ownInputDatums :: IsData a => ScriptContext  -> [a]
ownInputDatums ctx= mapMaybe (txOutDatum ctx) $  ownInputs ctx

-- get List of the parsed datums  including the TxOut if datum is valid
{-# INLINABLE ownInputsWithDatum #-}
ownInputsWithDatum:: IsData a =>  ScriptContext ->[(TxOut,a)]
ownInputsWithDatum ctx= mapMaybe (txOutWithDatum ctx)  ( ownInputs ctx)

-- get input datum for the utxo that is currently being validated
{-# INLINABLE ownInputDatum #-}
ownInputDatum :: IsData a => ScriptContext -> Maybe a
ownInputDatum ctx = do
    txInfo <-findOwnInput ctx
    let txOut= txInInfoResolved txInfo
    txOutDatum ctx txOut

--  given an Utxo, resolve it's datum to our type
{-# INLINABLE txOutDatum #-}
txOutDatum::  IsData a =>  ScriptContext ->TxOut -> Maybe a
txOutDatum ctx txOut =do
            dHash<-txOutDatumHash txOut
            datum<-findDatum dHash (scriptContextTxInfo ctx)
            PlutusTx.fromData (getDatum datum)

-- given txOut get resolve it to our type and return it with the txout
{-# INLINABLE txOutWithDatum #-}
txOutWithDatum::  IsData a =>  ScriptContext ->TxOut -> Maybe (TxOut,a)
txOutWithDatum ctx txOut =do
            d<-txOutDatum ctx txOut
            return (txOut,d)

--  value that is being redeemed from this contract in this utxo
{-# INLINABLE ownInputValue #-}
ownInputValue:: ScriptContext -> Value
ownInputValue ctx = case  findOwnInput ctx of
      Just TxInInfo{txInInfoResolved} ->  txOutValue txInInfoResolved

-- total value that will be locked by this contract in this transaction
{-# INLINABLE  ownOutputValue #-}
ownOutputValue :: ScriptContext -> Value
ownOutputValue ctx = valueLockedBy (scriptContextTxInfo ctx) (ownHash ctx)

