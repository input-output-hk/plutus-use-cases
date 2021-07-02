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
module Test.Validator.MarketValidatorTest
(tests)
where

import Test.TestHelper
import Control.Monad (void)
import Ledger.Ada ( lovelaceValueOf, adaSymbol, adaToken )
import Plutus.Contract.Test
import PlutusTx.Prelude
import Test.Tasty
import Plutus.Contract.Blockchain.MarketPlace
import Plutus.Contract.Wallet.EndpointModels
import Control.Lens hiding (to)
import Plutus.Trace.Emulator
import qualified Data.Aeson as AesonTypes
import Data.Text(Text)
import Ledger (TxOutRef, pubKeyHash, PubKeyHash (getPubKeyHash), Value, Address, ScriptContext, TxInInfo, Tx, Datum, TxOut, TxId)
import Prelude (show, String, IO, Integral (toInteger))
import Plutus.Contract.Blockchain.MarketPlace
import qualified Data.Map as Map
import qualified PlutusTx.AssocMap as AssocMap
import Ledger.Value(singleton, Value (Value))
import Ledger.Contexts
import Ledger.Interval hiding (singleton)
import Plutus.V1.Ledger.TxId (TxId(TxId))
import PlutusTx (IsData(toData))

--- It's WIP ..

tests :: TestTree
tests = testGroup "MarketValidator Unit Test"
      [ ]
type TestTx=[(Datum,Datum,ScriptContext,Datum->Datum->ScriptContext->Bool)]

type TInput =[(Value,Address)]

cannotBuyWithLesserValue= mkMarket defaultMarket  (toData $ toInteger 32) Buy ScriptContext{
    scriptContextTxInfo =TxInfo{
      txInfoInputs        =[] --[TxInInfo] -- ^ Transaction inputs
    , txInfoOutputs     =  [] -- [TxOut] -- ^ Transaction outputs
    , txInfoFee         =  defaultFee -- ^ The fee paid by this transaction.
    , txInfoForge       =  defaultForge -- ^ The 'Value' forged by this transaction.
    , txInfoDCert       = [] -- ^ Digests of certificates included in this transaction
    , txInfoWdrl        = [] -- ^ Withdrawals
    , txInfoValidRange  = to 32
 -- ^ The valid range for the transaction.
    , txInfoSignatories =  []--[PubKeyHash] -- ^ Signatures provided with the transaction, attested that they all signed the tx
    , txInfoData        =  []--[(DatumHash, Datum)]
    , txInfoId          = TxId "a"
    },
    scriptContextPurpose =Spending (TxOutRef (TxId "b") 0)
  }

defaultFee=singleton adaSymbol  adaToken  20
defaultForge=Value AssocMap.empty