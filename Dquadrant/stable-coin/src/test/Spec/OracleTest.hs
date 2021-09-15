{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Spec.OracleTest
  ( tests,
  )
where

import            Control.Lens
import            Control.Monad                       (void)
import qualified  Data.Map                            as Map
import            Data.Monoid                         (Last (..))
import            Data.Text                           hiding (all, length, reverse)
import            Ledger                              (pubKeyHash, txOutValue, txOutTxOut, pubKeyAddress, POSIXTimeRange, always)
import qualified  Ledger.Ada                          as Ada
import            Ledger.Address                      (Address)
import qualified  Ledger.Typed.Scripts                as Scripts
import            Ledger.Value                        (Value)
import            Ledger.Value                        as Value
import            Plutus.Contract                     as Contract
import            Plutus.Contract.Test
import qualified  Plutus.Trace.Emulator               as Trace
import qualified  PlutusTx  
import            PlutusTx.Numeric                    (negate, one, zero)
import            PlutusTx.Prelude
import qualified  PlutusTx.Prelude                    as PlutusTx
import            PlutusTx.Ratio                      as R
import            Test.Tasty
import            Test.Tasty.HUnit                  
import qualified  Prelude
import qualified  PlutusTx.Numeric                    as P
import           Plutus.Trace.Emulator.Types (_ContractLog, cilMessage)
import           Wallet.Emulator.MultiAgent  (eteEvent)


import Plutus.Contracts.Coins.CoinsStateMachine as CoinsMachine
import Plutus.Contracts.Coins.Types
import Plutus.Contracts.Coins.Endpoints
import Plutus.Contracts.Oracle.Core
import qualified Data.Aeson.Types as Types
-- import Utils.ValidatorTestFramework

oracleW1:: Wallet
oracleW1 = Wallet 1

--Currency symbol used in test for oracle
oracleSymbol :: CurrencySymbol
oracleSymbol = "ff"

--Default oracle paramter for oracle contract
oracle :: Oracle
oracle = Oracle
            { oNftSymbol = oracleSymbol
            , oOperator  = pubKeyHash $ walletPubKey oracleW1
            , oFee       = 1
            }

oValidatorAddress :: Address
oValidatorAddress = oracleAddress oracle

initialAdaValue :: Value
initialAdaValue = Ada.lovelaceValueOf 100_000_000

oracleToken :: Value
oracleToken =
    -- oracle symmbol "ff" is not a valid MPS hash. But this doesn't matter because we
    -- never try to forge any value of "ff" using a script.
    -- This currency is created by the initial transaction.
    Value.singleton oracleSymbol oracleTokenName 1

-- | 'CheckOptions' that inclues 'oracletoken' in the initial distribution of wallet 1.
options :: CheckOptions
options =
    let initialDistribution = Map.fromList
                                [ (oracleW1, initialAdaValue <> oracleToken)
                                ]

    in defaultCheckOptions & emulatorConfig . Trace.initialChainState .~ Left initialDistribution


tests :: TestTree
tests =
  testGroup
    "oracle"
    [ 
      checkPredicateOptions options "Update oracle"
          ( 
            (valueAtAddress oValidatorAddress (== oracleToken))
              .&&. 
              assertDone (checkOracle oracle) "checkOracle" (const True) "Updated oracle should be as expected"
              .&&. 
              assertNoFailedTransactions
          )
          $ updateOracleTrace newOracleValue
        
      ,  let  checkOracleLog :: Prelude.String
              checkOracleLog = "ok" in
                checkPredicateOptions options "Run oracle"
                  ( 
                assertNotDone runOracle "runOracle" "Should run oracle contract without errors"
                .&&. 
                endpointAvailable @"update" runOracle "runOracle"
                -- TODO check for log message for correct oracle
                -- .&&. 
                -- (assertInstanceLog "checkOracleToken" ((==) (Just checkOracleLog) . listToMaybe . reverse . mapMaybe (preview (eteEvent . cilMessage . _ContractLog))))
                .&&. 
                assertNoFailedTransactions
                )
          $ runOracleTrace
        
        -- execOracle "Can update oracle" (
        --   builderRedeem Update (Ada.lovelaceValueOf 1) newOracleValue
        -- )
    ]

newOracleValue :: Integer
newOracleValue = 3

type OracleContractHandle = (Trace.ContractHandle () OracleSchema Text)

initialiseOracle :: Trace.EmulatorTrace OracleContractHandle
initialiseOracle = do
  oracleHdl <- Trace.activateContractWallet oracleW1 $ runMockOracle oracle
  void $ Trace.waitNSlots 10

  Trace.callEndpoint @"update" oracleHdl 1
  void $ Trace.waitNSlots 10
  return oracleHdl

checkOracle :: Oracle -> Contract () OracleSchema Text ()
checkOracle oracle = do
    m <- findOracle oracle
    case m of
        Nothing        -> return ()
        Just (_, _, x) -> do
                        if(newOracleValue == x) then return ()
                        else throwError $ pack ("Error new Oracle value is not updated to " ++ Prelude.show newOracleValue ++ "but is " ++ Prelude.show x )

    void $ Contract.waitNSlots 1

updateOracleTrace :: Integer -> Trace.EmulatorTrace ()
updateOracleTrace newValue = do
  oracleHdl <- initialiseOracle
  Trace.callEndpoint @"update" oracleHdl newValue
  void $ Trace.waitNSlots 10

  void $ Trace.activateContract oracleW1 (checkOracle oracle) "checkOracle"
  void $ Trace.waitNSlots 10

checkWalletHasOracleToken :: Oracle -> Contract () OracleSchema Text ()
checkWalletHasOracleToken Oracle{oNftSymbol}= do
    pk    <- ownPubKey
    utxos <- utxoAt $ pubKeyAddress pk
    let xs = [ (oref, o)
             | (oref, o) <- Map.toList utxos
             , Value.valueOf (txOutValue $ txOutTxOut o) oNftSymbol oracleTokenName == 1
             ]

    case xs of
      [(oref, o)] -> do
                  logInfo @Prelude.String "Check oracle token passed"
                  return ()
      _ -> throwError "Oracle token not found at wallet"

getOracle :: Trace.ContractHandle (Last Oracle) OracleSchema Text -> Trace.EmulatorTrace Oracle
getOracle h = do
        l <- Trace.observableState h
        case l of
            Last Nothing       -> Trace.waitNSlots 1 >> getOracle h
            Last (Just oracle) -> return oracle

runOracleTrace :: Trace.EmulatorTrace ()
runOracleTrace = do
  hdl <- Trace.activateContract oracleW1 runOracle "runOracle"
  void $ Trace.waitNSlots 10
  oracle <- getOracle hdl
  void $ Trace.activateContract oracleW1 (checkWalletHasOracleToken oracle) "checkOracleToken"
  void $ Trace.waitNSlots 10
  Trace.callEndpoint @"update" hdl 1
  void $ Trace.waitNSlots 10


-- execOracle :: TestName-> TestContextBuilder -> TestTree
-- execOracle testName  ctx = execOracleTimed testName ctx  always

-- execOracleTimed :: TestName-> TestContextBuilder -> POSIXTimeRange -> TestTree
-- execOracleTimed name ctx range =testCase name (executeSpendContext  _oracleValidator ctx range @?= True)
--   where
--     _oracleValidator d r ctx= case PlutusTx.fromData r of
--       Just redeemer -> case PlutusTx.fromData d of 
--                           Just dat -> mkOracleValidator oracle dat redeemer ctx
--                           _ -> False
--       _     -> False
