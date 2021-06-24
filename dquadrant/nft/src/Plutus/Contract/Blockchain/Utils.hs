{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Plutus.Contract.Blockchain.Utils
where
import PlutusTx.Prelude
import Ledger.Value as Value ( Value )
import           Ledger.Contexts          (ScriptContext (..), TxInfo (..))
import Ledger  hiding(txOutDatum)
import PlutusTx ( IsData(fromData) )

{-# INLINABLE ownAddress #-}
ownAddress :: ScriptContext -> Address
ownAddress ctx=scriptHashAddress (ownHash ctx)

{-# INLINABLE  ownInputs #-}
ownInputs:: ScriptContext -> [TxOut]
ownInputs ctx@ScriptContext{scriptContextTxInfo=TxInfo{txInfoInputs}}=
     filter (\x->txOutAddress x==ownAddress ctx) resolved
    where
    resolved=map (\x->txInInfoResolved x) txInfoInputs

ownInputDatums :: IsData a => ScriptContext  -> [a]
ownInputDatums ctx= mapMaybe (txOutDatum ctx) $  ownInputs ctx

{-# INLINABLE ownInputDatum #-}
ownInputDatum :: IsData a => ScriptContext -> Maybe a
ownInputDatum ctx = do
    txInfo <-findOwnInput ctx
    let txOut= txInInfoResolved txInfo
    txOutDatum ctx txOut

{-# INLINABLE txOutDatum #-}
txOutDatum::  IsData a =>  ScriptContext ->TxOut -> Maybe a
txOutDatum ctx txOut =do
            dHash<-txOutDatumHash txOut
            datum<-findDatum dHash (scriptContextTxInfo ctx)
            PlutusTx.fromData (getDatum datum)

{-# INLINABLE ownInputValue #-}
ownInputValue:: ScriptContext -> Value
ownInputValue ctx = case  findOwnInput ctx of
      Just TxInInfo{txInInfoResolved} ->  txOutValue txInInfoResolved

{-# INLINABLE  ownOutputValue #-}
ownOutputValue :: ScriptContext -> Value
ownOutputValue ctx = valueLockedBy (scriptContextTxInfo ctx) (ownHash ctx)

