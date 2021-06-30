{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Spec.StableCoinTest
  ( tests,
  )
where

import            Control.Lens
import            Control.Monad                       (void)
import qualified  Data.Map                            as Map
import qualified  Data.Monoid                         ((<>))
import            Data.Text                           hiding (all, length)
import            Ledger                              (pubKeyHash)
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
import qualified  Test.Tasty.HUnit                    as HUnit
import qualified  Prelude
import qualified  PlutusTx.Numeric                    as P

import Plutus.Contracts.Coins.CoinsStateMachine as CoinsMachine
import Plutus.Contracts.Coins.Types
import Plutus.Contracts.Coins.Endpoints
import Plutus.Contracts.Oracle.Core
import qualified Data.Aeson.Types as Types

oracleW1, w2 ,w3:: Wallet
oracleW1 = Wallet 1
w2 =       Wallet 2
w3 =       Wallet 3

stableCoinName :: TokenName
stableCoinName = "StableToken"

reserveCoinName :: TokenName
reserveCoinName = "ReserveToken"

oracleSymbol :: CurrencySymbol
oracleSymbol = "ff"

oracle :: Oracle
oracle = Oracle
            { oNftSymbol = oracleSymbol
            , oOperator  = pubKeyHash $ walletPubKey oracleW1
            , oFee       = 1
            }

bp :: BankParam
bp = BankParam
            { 
            stableCoinTokenName = stableCoinName,
            reserveCoinTokenName = reserveCoinName,
            minReserveRatio = P.zero,
            maxReserveRatio = 4 % 1,
            rcDefaultRate = 1,
            oracleParam = oracle,
            oracleAddr = oracleAddress oracle
            }

coinsMachineAddress :: Address
coinsMachineAddress = Scripts.validatorAddress $ CoinsMachine.scriptInstance bp

oValidatorAddress :: Address
oValidatorAddress = oracleAddress oracle

reserveCoinsValue :: BankParam -> Integer -> Value
reserveCoinsValue bankParam@BankParam {reserveCoinTokenName} tokenAmount =
  let mpHash = Scripts.forwardingMonetaryPolicyHash $ CoinsMachine.scriptInstance bankParam
   in Value.singleton (Value.mpsSymbol mpHash) reserveCoinTokenName tokenAmount

stableCoinsValue :: BankParam -> Integer -> Value
stableCoinsValue bankParam@BankParam {stableCoinTokenName} tokenAmount =
  let mpHash = Scripts.forwardingMonetaryPolicyHash $ CoinsMachine.scriptInstance bankParam
   in Value.singleton (Value.mpsSymbol mpHash) stableCoinTokenName tokenAmount

adaVal :: Integer -> Value
adaVal x = Ada.lovelaceValueOf x

initialAdaValue :: Value
initialAdaValue = adaVal 100_000_000

oracleFee :: Value
oracleFee = adaVal $ oFee oracle

--value of oracle for getting multiple time oracle used in same test
oracleFeeMultiply :: Integer -> Value
oracleFeeMultiply mulBy = adaVal (oFee oracle * mulBy)

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
                                  , (w2, initialAdaValue)
                                ]

    in defaultCheckOptions & emulatorConfig . Trace.initialChainState .~ Left initialDistribution


emCfg :: Trace.EmulatorConfig
emCfg = Trace.EmulatorConfig $ Left $ Map.fromList [(Wallet 1, v), (Wallet 2, v)]
 where
  v :: Value
  v = Ada.lovelaceValueOf 100_000_000

tests :: TestTree
tests =
  testGroup
    "stablecoin"
    [ 
      checkPredicateOptions options "mint stablecoins"
          ( 
            (valueAtAddress coinsMachineAddress (== (adaVal 100)))
              .&&. 
              assertNoFailedTransactions
              .&&. walletFundsChange w2 ((stableCoinsValue bp 100) <> (negate (adaVal 100)) <> (negate oracleFee))
          )
      $ mintStableCoins 100
    ,
      checkPredicateOptions options "mint reservecoins"
        ( (valueAtAddress coinsMachineAddress (== (adaVal 100)))
            .&&. assertNoFailedTransactions
            .&&. walletFundsChange w2 ((reserveCoinsValue bp 100) <> (negate (adaVal 100)) <> (negate oracleFee))
        )
        $ mintReserveCoins 100
        
        --Mint 10 stable coin, redeem 5 stable coin
        -- So value at coinsMachineAddress is 5 and user also have  
        -- 5 stable coins and only 5 ada is cutoff at final wallet balances with rate 1:1
    ,   -- Oracle fee * 2 as double fee is required for minting and redeeming
      checkPredicateOptions options "mint stablecoins redeem stablecoins"
        ( ( valueAtAddress coinsMachineAddress (== (adaVal 5)))
            .&&. assertNoFailedTransactions
            .&&. walletFundsChange w2 ((stableCoinsValue bp 5) <> (negate (adaVal 5)) <> (negate (oracleFeeMultiply 2)))
        )
        $ mintAndRedeemStableCoins 10 5
    ,    
      checkPredicateOptions options "mint reservecoins redeem reservecoins"
        ( ( valueAtAddress coinsMachineAddress (== (adaVal 5)))
            .&&. assertNoFailedTransactions
            .&&. walletFundsChange w2 ((reserveCoinsValue bp 5) <> (negate (adaVal 5)) <> (negate (oracleFeeMultiply 2)))
        )
        $ mintAndRedeemReserveCoins 10 5
    ,
        --TODO improve on error testing to test specific log message of error
        checkPredicateOptions options "mint stablecoins try redeem more stablecoins than minted should fail"
        (  
           assertContractError (coinsContract bp) (Trace.walletInstanceTag w2) (\_ -> True) "should throw insufficent funds"
        )
        $ mintAndRedeemStableCoins 10 15
    ,
        checkPredicateOptions options "mint reserve coins try redeem more reserve coins than minted should fail"
        (  
           assertContractError (coinsContract bp) (Trace.walletInstanceTag w2) (\_ -> True) "should throw insufficent funds"
        )
        $ mintAndRedeemReserveCoins 10 15
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

type CoinsContractHandle = (Trace.ContractHandle [Types.Value ] BankStateSchema Text)

initialise :: Trace.EmulatorTrace CoinsContractHandle
initialise = do
  oracleHdl <- Trace.activateContractWallet oracleW1 $ runMockOracle oracle
  void $ Trace.waitNSlots 10

  Trace.callEndpoint @"update" oracleHdl 1
  void $ Trace.waitNSlots 10

  hdl <- Trace.activateContractWallet w2 $ coinsContract bp
  
  let i = 1 :: Integer
  Trace.callEndpoint @"start" hdl i
  _ <- Trace.waitNSlots 2
  return hdl

mintStableCoins :: Integer -> Trace.EmulatorTrace ()
mintStableCoins tokenAmount = do
  hdl <- initialise
  void $ callStableMintEndpoint tokenAmount hdl

callStableMintEndpoint :: Integer -> CoinsContractHandle -> Trace.EmulatorTrace ()
callStableMintEndpoint tokenAmount hdl = do
  Trace.callEndpoint @"mintStableCoin"
    hdl
    EndpointInput
      { 
        tokenAmount = tokenAmount
      }
  void $ Trace.waitNSlots 2

mintReserveCoins :: Integer -> Trace.EmulatorTrace ()
mintReserveCoins tokenAmount = do
  hdl <- initialise
  void $ callReserveMintEndpoint tokenAmount hdl

callReserveMintEndpoint :: Integer -> CoinsContractHandle -> Trace.EmulatorTrace ()
callReserveMintEndpoint tokenAmount hdl = do
  Trace.callEndpoint @"mintReserveCoin"
    hdl
    EndpointInput
      { 
        tokenAmount = tokenAmount
      }
  void $ Trace.waitNSlots 2

mintAndRedeemStableCoins :: Integer -> Integer -> Trace.EmulatorTrace ()
mintAndRedeemStableCoins tokenAmountToMint tokenAmountToRedeem = do
  hdl <- initialise
  void $ callStableMintEndpoint tokenAmountToMint hdl

  Trace.callEndpoint @"redeemStableCoin"
      hdl
      EndpointInput
        {
          tokenAmount = tokenAmountToRedeem
        }
  void $ Trace.waitNSlots 2

mintAndRedeemReserveCoins :: Integer -> Integer -> Trace.EmulatorTrace ()
mintAndRedeemReserveCoins tokenAmountToMint tokenAmountToRedeem = do
  hdl <- initialise
  void $ callReserveMintEndpoint tokenAmountToMint hdl

  Trace.callEndpoint @"redeemReserveCoin"
      hdl
      EndpointInput
        {
          tokenAmount = tokenAmountToRedeem
        }
  void $ Trace.waitNSlots 2