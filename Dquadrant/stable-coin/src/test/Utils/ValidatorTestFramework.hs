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
module Utils.ValidatorTestFramework

where

import Ledger.Ada ( lovelaceValueOf, adaSymbol, adaToken )
import Plutus.Contract.Test
import PlutusTx.Prelude
import Ledger hiding(value,signatures)
import qualified PlutusTx.AssocMap as AssocMap
import PlutusTx (toData, Data)
import PlutusTx.IsData
import Ledger.Credential ( Credential(ScriptCredential) )
import Ledger.Value
import GHC.Show (Show)

--TODO upgrade for plutus dependencies v1.0.8
-- data TestCtxIn  =  PkhCtxIn (Address,Value) | ScriptCtxIn (ByteString,Value,Data) |ThisScripCtxIn(Value, Data,Data) deriving (Show)
-- data TestCtxOut = PkhCtxOut (Address,Value) | ScriptCtxOut (ByteString,Value,Data) |ThisScripCtxOut(Value,Data) deriving (Show)


-- data TestContextBuilder=TestContextBuilder{
--     ctxInputs:: [TestCtxIn],
--     ctxOutputs::[TestCtxOut],
--     ctxSignatures ::[PubKeyHash]
--   } deriving(Show)
-- instance Semigroup TestContextBuilder where
--   (<>) ctx1 ctx2=TestContextBuilder{
--     ctxInputs=ctxInputs ctx1 ++ ctxInputs ctx2,
--     ctxOutputs=ctxOutputs ctx1 ++ ctxOutputs ctx2,
--     ctxSignatures=ctxSignatures ctx1 ++ctxSignatures ctx2
--   }

-- builderPayTo::Wallet ->Value ->TestContextBuilder
-- builderPayTo w v= TestContextBuilder{
--     ctxInputs=[],
--     ctxOutputs=[PkhCtxOut  (pubKeyAddress $ walletPubKey w,v)],
--     ctxSignatures=[]
--   }

-- builderPayLovelaceTo :: Wallet -> Integer -> TestContextBuilder
-- builderPayLovelaceTo w v=builderPayTo w (lovelaceValueOf v)

-- builderLockInThisScript:: IsData a=> a->Value->TestContextBuilder
-- builderLockInThisScript d v=TestContextBuilder{
--     ctxInputs=[],
--     ctxOutputs=[ThisScripCtxOut ( v, toData d)],
--     ctxSignatures=[]
--   }

-- builderLockInScript:: IsData a=>ByteString -> a -> Value->TestContextBuilder
-- builderLockInScript bs d v =TestContextBuilder{
--     ctxInputs=[],
--     ctxOutputs=[ScriptCtxOut (bs,v,toData d)],
--     ctxSignatures=[]
--   }

-- builderSpend:: Wallet ->Value ->TestContextBuilder
-- builderSpend w v=TestContextBuilder{
--     ctxInputs=[PkhCtxIn (pubKeyAddress $ walletPubKey w,v)],
--     ctxOutputs=[],
--     ctxSignatures=[pubKeyHash $ walletPubKey w]
--   }


-- builderRedeem:: (IsData a,IsData b)=>b-> Value->a->TestContextBuilder
-- builderRedeem r v d=TestContextBuilder{
--     ctxInputs=[ThisScripCtxIn (v,toData d,toData r)],
--     ctxOutputs=[],
--     ctxSignatures=[]
--   }

-- builderRedeemAnotherScript:: Value ->TestContextBuilder
-- builderRedeemAnotherScript v =TestContextBuilder{
--     ctxInputs=[ThisScripCtxIn (v,toData (),toData ())],
--     ctxOutputs=[],
--     ctxSignatures=[]
-- }


-- executeSpendContext :: 
--   (Data -> Data -> ScriptContext -> Bool)
--   -> TestContextBuilder -> POSIXTimeRange -> Bool
-- executeSpendContext f ctx range =executeContext f ctx range defaultForge

-- executeContext :: (Data->Data->ScriptContext ->Bool)->TestContextBuilder-> POSIXTimeRange ->Value ->Bool
-- executeContext f (TestContextBuilder cInputs cOutputs signatures) timeRange forge=
--         all (\(tin,i)-> applyRedeemer tin i) $ indexedInputs
--   where

--     applyRedeemer tin index= case tin of
--       ThisScripCtxIn(_,_data,redeemer) ->  f _data  redeemer  (ctx $ Spending (TxOutRef testSourceTxHash index))
--       _ -> True

--     toOutput tout=case tout of
--       PkhCtxOut (addr,value) -> TxOut addr value Nothing
--       ScriptCtxOut (sAddr,value,_data)-> TxOut (Address ( ScriptCredential ( ValidatorHash sAddr)) Nothing) value (Just $ datumHash $   Datum  _data)
--       ThisScripCtxOut(value,_data) -> TxOut (Address ( ScriptCredential ( ValidatorHash "")) Nothing) value (Just $ datumHash $   Datum  _data)


--     toInput tin=case tin of
--       PkhCtxIn (addr,value) -> TxOut addr value Nothing
--       ScriptCtxIn (sAddr,value,_data) -> TxOut (Address ( ScriptCredential ( ValidatorHash sAddr)) Nothing) value (Just $ datumHash $   Datum  _data)
--       ThisScripCtxIn(value, _data,_) -> TxOut (Address ( ScriptCredential ( ValidatorHash "")) Nothing) value (Just $ datumHash $   Datum  _data)

--     toInputDatum tin=case tin of
--       ScriptCtxIn (_,_,_data) -> Just (datumHash $   Datum  _data, Datum _data)
--       ThisScripCtxIn (_,_data,_) -> Just (datumHash $   Datum  _data, Datum _data)
--       _ -> Nothing

--     toOutputDatum tout=case tout of
--       ScriptCtxOut (_,_,_data) -> Just  (datumHash $   Datum  _data, Datum _data)
--       ThisScripCtxOut (_,_data)->Just  (datumHash $   Datum  _data ,Datum _data)
--       _ ->Nothing

--     indexedInputs=zip cInputs [1..]

--     ctx purpose=ScriptContext{
--       scriptContextTxInfo =TxInfo{
--         txInfoInputs        =map (\(v,i)->TxInInfo (TxOutRef testSourceTxHash i) (toInput v) ) indexedInputs
--       , txInfoOutputs     =  map toOutput cOutputs -- [TxOut] -- ^ Transaction outputs
--       , txInfoFee         =  defaultFee --  ^ The fee paid by this transaction.
--       , txInfoForge       =  forge -- Value ^ The 'Value' forged by this transaction.
--       , txInfoDCert       = [] -- [_] ^ Digests of certificates included in this transaction
--       , txInfoWdrl        = [] -- [_] ^ Withdrawals
--       , txInfoValidRange  = timeRange-- [_] ^ The valid range for the transaction.
--       , txInfoSignatories =  signatures--[PubKeyHash] -- ^ Signatures provided with the transaction, attested that they all signed the tx
--       , txInfoData        =  mapMaybe toOutputDatum cOutputs ++ mapMaybe toInputDatum cInputs
--       , txInfoId          = TxId "testtx"
--       },
--       scriptContextPurpose =purpose
--     }

-- defaultFee :: Value
-- defaultFee=Ledger.Value.singleton adaSymbol  adaToken  43
-- defaultForge :: Value
-- defaultForge=Value AssocMap.empty

-- testSourceTxHash :: TxId
-- testSourceTxHash=TxId  "abcd"

