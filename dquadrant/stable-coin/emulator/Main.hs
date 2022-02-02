{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (void)
import Data.Default (Default (def))
import qualified Data.Map as Map
import Data.Text hiding (all, length, reverse, singleton)
import qualified Ledger.Ada as Ada
import Ledger.Value
import Plutus.Contract as Contract
import Plutus.Contracts.Oracle.Core
import Plutus.Trace.Emulator hiding (throwError)
import qualified Plutus.Trace.Emulator as Trace
import Wallet.Emulator.Wallet
import Config

oracleW1 :: Wallet
oracleW1 = knownWallet 1

--Default oracle paramter for oracle contract
oracle :: Oracle
oracle =
  Oracle
    { oNftSymbol = oracleSymbol,
      oOperator = mockWalletPaymentPubKeyHash oracleW1,
      oFee = 1
    }

newOracleValue :: Integer
newOracleValue = 3

--Currency symbol used in test for oracle
oracleSymbol :: CurrencySymbol
oracleSymbol = "ff"

oracleToken :: Value
oracleToken =
  -- oracle symmbol "ff" is not a valid MPS hash. But this doesn't matter because we
  -- never try to forge any value of "ff" using a script.
  singleton oracleSymbol oracleTokenName 1

main :: IO ()
main = do
  runTrace $ updateOracleTrace newOracleValue

emulatorCfg :: Trace.EmulatorConfig
emulatorCfg = EmulatorConfig (Left $ Map.fromList ([(knownWallet i, v) | i <- [1 .. 4]])) def def
  where
    v = Ada.lovelaceValueOf 100_000_000 <> oracleToken

runTrace :: Trace.EmulatorTrace () -> IO ()
runTrace = Trace.runEmulatorTraceIO' customTraceConfig emulatorCfg

type OracleContractHandle = (Trace.ContractHandle () OracleSchema Text)

initialiseOracle :: Trace.EmulatorTrace OracleContractHandle
initialiseOracle = do
  oracleHdl <- Trace.activateContractWallet oracleW1 $ runMockOracle oracle
  void $ Trace.waitNSlots 10

  Trace.callEndpoint @"update" oracleHdl 1
  void $ Trace.waitNSlots 10
  return oracleHdl

updateOracleTrace :: Integer -> Trace.EmulatorTrace ()
updateOracleTrace newValue = do
  oracleHdl <- initialiseOracle
  Trace.callEndpoint @"update" oracleHdl newValue
  void $ Trace.waitNSlots 10

  void $ Trace.activateContract oracleW1 (checkOracle oracle) "checkOracle"
  void $ Trace.waitNSlots 10

checkOracle :: Oracle -> Contract () OracleSchema Text ()
checkOracle oracleParam = do
  m <- findOracle oracleParam
  case m of
    Nothing -> return ()
    Just (_, _, x) -> do
      if (newOracleValue == x)
        then do
          logInfo @String "Check updated oracle passed"
          return ()
        else throwError $ pack ("Error new Oracle value is not updated to " ++ Prelude.show newOracleValue ++ "but is " ++ Prelude.show x)

  void $ Contract.waitNSlots 1
