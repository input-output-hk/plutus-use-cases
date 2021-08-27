{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores#-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Test.ValidatorTestFramework
-- (tests)
where

import Ledger.Ada ( lovelaceValueOf, adaSymbol, adaToken )
import Plutus.Contract.Test
import PlutusTx.Prelude
import Ledger hiding(value,signatures)
import qualified PlutusTx.AssocMap as AssocMap
import PlutusTx (IsData(toData), Data)
import Ledger.Credential ( Credential(ScriptCredential) )
import Ledger.Value
import GHC.Show (Show)

data TestCtxIn  =  PkhCtxIn (Address,Value) | ScriptCtxIn (ByteString,Value,Data) |ThisScripCtxIn(Value, Data,Data) deriving (Show)
data TestCtxOut = PkhCtxOut (Address,Value) | ScriptCtxOut (ByteString,Value,Data) |ThisScripCtxOut(Value,Data) deriving (Show)


-- Context Builder for test transaction
-- You will not use this directly, instead use builder functions
-- to compose this structure.
data TestContextBuilder=TestContextBuilder{
    ctxInputs:: [TestCtxIn], -- inputs in this transaction
    ctxOutputs::[TestCtxOut], -- outputs in this transaction
    ctxSignatures ::[PubKeyHash] -- public key signatures in this transaction
  } deriving(Show)
instance Semigroup TestContextBuilder where
  (<>) ctx1 ctx2=TestContextBuilder{
    ctxInputs=ctxInputs ctx1 ++ ctxInputs ctx2,
    ctxOutputs=ctxOutputs ctx1 ++ ctxOutputs ctx2,
    ctxSignatures=ctxSignatures ctx1 ++ctxSignatures ctx2
  }

-- In this transaction, pay some value to the wallet.
-- It will be included in the tx_out of this transaction
builderPayTo::Wallet ->Value ->TestContextBuilder
builderPayTo w v= TestContextBuilder{
    ctxInputs=[],
    ctxOutputs=[PkhCtxOut  (pubKeyAddress $ walletPubKey w,v)],
    ctxSignatures=[]
  }

-- In this transaction send  x lovelace to an wallet.
-- It will appear in tx_out
builderPayLovelaceTo :: Wallet -> Integer -> TestContextBuilder
builderPayLovelaceTo w v=builderPayTo w (lovelaceValueOf v)

-- Lock value and data in the script that is being validated.
-- This is the validator script we are currently testing
builderLockInThisScript:: IsData _data=>Value -> _data->TestContextBuilder
builderLockInThisScript v _data =TestContextBuilder{
    ctxInputs=[],
    ctxOutputs=[ThisScripCtxOut ( v, toData _data)],
    ctxSignatures=[]
  }

-- Lock value and data in a script.
-- It's a script that we depend on. but we are not testing it.
-- So, the validator of this script will not be executed.
builderLockInScript:: IsData _data=>ByteString -> _data -> Value->TestContextBuilder
builderLockInScript bs _data v =TestContextBuilder{
    ctxInputs=[],
    ctxOutputs=[ScriptCtxOut (bs,v,toData _data)],
    ctxSignatures=[]
  }

-- Spend Value from a wallet.
-- Since spending requires signature, It will also add
-- signature of that wallet
builderSpend:: Wallet ->Value ->TestContextBuilder
builderSpend w v=TestContextBuilder{
    ctxInputs=[PkhCtxIn (pubKeyAddress $ walletPubKey w,v)],
    ctxOutputs=[],
    ctxSignatures=[pubKeyHash $ walletPubKey w]
  }


-- Redeem from Script Address.
builderRedeem:: (IsData _data,IsData redeemer)=>redeemer-> Value->_data->TestContextBuilder
builderRedeem redeemer value _data=TestContextBuilder{
    ctxInputs=[ThisScripCtxIn (value,toData _data,toData redeemer)],
    ctxOutputs=[],
    ctxSignatures=[]
  }

-- When we are redeeming from another script, we are not interested in it's data or redeemer
-- we can just ignore it.
builderRedeemAnotherScript:: Value ->TestContextBuilder
builderRedeemAnotherScript v =TestContextBuilder{
    ctxInputs=[ThisScripCtxIn (v,toData (),toData ())],
    ctxOutputs=[],
    ctxSignatures=[]
}

-- Add signature of a wallet to this transaction.
-- Note that when you are spending from wallet, signature is included
-- by default. But there are cases when you are not spending
-- from wallet and require signature.
builderSign:: Wallet -> TestContextBuilder
builderSign w=TestContextBuilder{
  ctxInputs=[],
  ctxOutputs=[],
  ctxSignatures=[pubKeyHash $ walletPubKey w]
}


-- Given a validtator function and ContextBuilder object
-- execute it and give boolean result.
-- note that it won't throw exceptions or stuffs.
-- the result can be asserted only based on the return value.
executeSpendContext ::
  (Data -> Data -> ScriptContext -> Bool)
  -> TestContextBuilder -> POSIXTimeRange -> Bool
executeSpendContext f ctx range =executeContext f ctx range defaultForge

executeContext :: (Data->Data->ScriptContext ->Bool)->TestContextBuilder-> POSIXTimeRange ->Value ->Bool
executeContext f (TestContextBuilder cInputs cOutputs signatures) timeRange forge=
        all (\(tin,i)-> applyRedeemer tin i) $ indexedInputs
  where

    applyRedeemer tin index= case tin of
      ThisScripCtxIn(_,_data,redeemer) ->  f _data  redeemer  (ctx $ Spending (TxOutRef testSourceTxHash index))
      _ -> True

    toOutput tout=case tout of
      PkhCtxOut (addr,value) -> TxOut addr value Nothing
      ScriptCtxOut (sAddr,value,_data)-> TxOut (Address ( ScriptCredential ( ValidatorHash sAddr)) Nothing) value (Just $ datumHash $   Datum  _data)
      ThisScripCtxOut(value,_data) -> TxOut (Address ( ScriptCredential ( ValidatorHash "")) Nothing) value (Just $ datumHash $   Datum  _data)


    toInput tin=case tin of
      PkhCtxIn (addr,value) -> TxOut addr value Nothing
      ScriptCtxIn (sAddr,value,_data) -> TxOut (Address ( ScriptCredential ( ValidatorHash sAddr)) Nothing) value (Just $ datumHash $   Datum  _data)
      ThisScripCtxIn(value, _data,_) -> TxOut (Address ( ScriptCredential ( ValidatorHash "")) Nothing) value (Just $ datumHash $   Datum  _data)

    toInputDatum tin=case tin of
      ScriptCtxIn (_,_,_data) -> Just (datumHash $   Datum  _data, Datum _data)
      ThisScripCtxIn (_,_data,_) -> Just (datumHash $   Datum  _data, Datum _data)
      _ -> Nothing

    toOutputDatum tout=case tout of
      ScriptCtxOut (_,_,_data) -> Just  (datumHash $   Datum  _data, Datum _data)
      ThisScripCtxOut (_,_data)->Just  (datumHash $   Datum  _data ,Datum _data)
      _ ->Nothing

    indexedInputs=zip cInputs [1..]

    ctx purpose=ScriptContext{
      scriptContextTxInfo =TxInfo{
        txInfoInputs        =map (\(v,i)->TxInInfo (TxOutRef testSourceTxHash i) (toInput v) ) indexedInputs
      , txInfoOutputs     =  map toOutput cOutputs -- [TxOut] -- ^ Transaction outputs
      , txInfoFee         =  defaultFee --  ^ The fee paid by this transaction.
      , txInfoForge       =  forge -- Value ^ The 'Value' forged by this transaction.
      , txInfoDCert       = [] -- [_] ^ Digests of certificates included in this transaction
      , txInfoWdrl        = [] -- [_] ^ Withdrawals
      , txInfoValidRange  = timeRange-- [_] ^ The valid range for the transaction.
      , txInfoSignatories =  signatures--[PubKeyHash] -- ^ Signatures provided with the transaction, attested that they all signed the tx
      , txInfoData        =  mapMaybe toOutputDatum cOutputs ++ mapMaybe toInputDatum cInputs
      , txInfoId          = TxId "testtx"
      },
      scriptContextPurpose =purpose
    }

defaultFee :: Value
defaultFee=Ledger.Value.singleton adaSymbol  adaToken  0
defaultForge :: Value
defaultForge=Value AssocMap.empty

testSourceTxHash :: TxId
testSourceTxHash=TxId  "abcd"

-- a=TxInInfo

