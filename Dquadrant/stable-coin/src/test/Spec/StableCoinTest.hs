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

import Control.Monad (void)
import Control.Monad.Freer.Extras as Extras
import qualified Data.Map as Map
import Data.Text
import qualified Ledger.Ada as Ada
import Ledger.Address (Address)
import Ledger.Typed.Scripts (scriptAddress)
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value (Value)
import Ledger.Value as Value
import Plutus.Contract
import Plutus.Contract.Test
import qualified Plutus.Contracts.CoinsStateMachine as CoinsMachine
import qualified Plutus.Trace.Emulator as Trace
import qualified PlutusTx
import PlutusTx.Numeric (negate, one, zero)
import PlutusTx.Prelude
import qualified PlutusTx.Prelude as PlutusTx
import PlutusTx.Ratio as R
import Test.Tasty
import qualified Test.Tasty.HUnit as HUnit

w1, w2 :: Wallet
w1 = Wallet 1
w2 = Wallet 2

t1, t2 :: Trace.ContractInstanceTag
t1 = Trace.walletInstanceTag w1
t2 = Trace.walletInstanceTag w2

emCfg :: Trace.EmulatorConfig
emCfg = Trace.EmulatorConfig $ Left $ Map.fromList [(Wallet w, v) | w <- [1, 3]]
  where
    v :: Value
    v = Ada.lovelaceValueOf 1000_000_000

stableCoinName :: TokenName
stableCoinName = "StableToken"

reserveCoinName :: TokenName
reserveCoinName = "ReserveToken"

bp :: CoinsMachine.BankParam
bp =
  CoinsMachine.BankParam
    { stableCoinTokenName = stableCoinName,
      reserveCoinTokenName = reserveCoinName,
      minReserveRatio = zero,
      maxReserveRatio = 4 % 1,
      rcDefaultRate = 1 % 1
    }

coinsMachineAddress :: Address
coinsMachineAddress = scriptAddress $ CoinsMachine.scriptInstance bp

initialAdaValue :: Value
initialAdaValue = Ada.lovelaceValueOf 100

reserveCoinsValue :: CoinsMachine.BankParam -> Integer -> Value
reserveCoinsValue bankParam@CoinsMachine.BankParam {reserveCoinTokenName} tokenAmount =
  let mpHash = Scripts.monetaryPolicyHash $ CoinsMachine.scriptInstance bankParam
   in Value.singleton (Value.mpsSymbol mpHash) reserveCoinTokenName tokenAmount

stableCoinsValue :: CoinsMachine.BankParam -> Integer -> Value
stableCoinsValue bankParam@CoinsMachine.BankParam {stableCoinTokenName} tokenAmount =
  let mpHash = Scripts.monetaryPolicyHash $ CoinsMachine.scriptInstance bankParam
   in Value.singleton (Value.mpsSymbol mpHash) stableCoinTokenName tokenAmount

tests :: TestTree
tests =
  testGroup
    "stablecoin"
    [ 
    --   checkPredicate "mint stablecoins"
    --     ( (valueAtAddress coinsMachineAddress (== initialAdaValue))
    --         .&&. assertNoFailedTransactions
    --         .&&. walletFundsChange w1 ((stableCoinsValue bp 100) <> (negate initialAdaValue))
    --     )
    --     $ mintStableCoins 100 1 1
    -- ,  
    --   checkPredicate "mint reservecoins"
    --     ( (valueAtAddress coinsMachineAddress (== initialAdaValue))
    --         .&&. assertNoFailedTransactions
    --         .&&. walletFundsChange w1 ((reserveCoinsValue bp 100) <> (negate initialAdaValue))
    --     )
    --     $ mintReserveCoins 100 1 1
        
    --     --Mint 10 stable coin, redeem 5 stable coin
    --     -- So value at address is 5 and user also have  stable coin with charge of 10 at rate 1:1
    -- , 
    --   checkPredicate "mint stablecoins redeem stablecoins"
    --     ( ( valueAtAddress coinsMachineAddress (== (Ada.lovelaceValueOf 5)))
    --         .&&. assertNoFailedTransactions
    --         .&&. walletFundsChange w1 ((stableCoinsValue bp 5) <> (negate (Ada.lovelaceValueOf 5)))
    --     )
    --     $ mintAndRedeemStableCoins 10 5 1 1
    -- ,    
    --   checkPredicate "mint reservecoins redeem reservecoins"
    --     ( ( valueAtAddress coinsMachineAddress (== (Ada.lovelaceValueOf 5)))
    --         .&&. assertNoFailedTransactions
    --         .&&. walletFundsChange w1 ((reserveCoinsValue bp 5) <> (negate (Ada.lovelaceValueOf 5)))
    --     )
    --     $ mintAndRedeemReserveCoins 10 5 1 1
    -- ,    
        -- checkPredicate "mint stablecoins try redeem more stablecoins than minted should fail"
        -- ( ( valueAtAddress coinsMachineAddress (== (Ada.lovelaceValueOf 5)))
        --     .&&. assertFailedTransaction (\_ err _ -> False)
        --     .&&. walletFundsChange w1 ((stableCoinsValue bp 10) <> (negate (Ada.lovelaceValueOf 10)))
        -- )
        -- $ mintAndRedeemStableCoins 10 15 1 1
    

        
    ]

initialise :: Trace.EmulatorTrace (Trace.ContractHandle () CoinsMachine.BankStateSchema Text)
initialise = do
  hdl <- Trace.activateContractWallet w1 CoinsMachine.endpoints
  let i = 5 :: Integer
  Trace.callEndpoint @"start" hdl i
  _ <- Trace.waitNSlots 2
  Extras.logInfo @String "Callled initialise"
  return hdl

mintStableCoins :: Integer -> Integer -> Integer -> Trace.EmulatorTrace ()
mintStableCoins tokenAmount rateNume rateDeno = do
  hdl <- initialise
  Trace.callEndpoint @"mintStableCoin"
    hdl
    CoinsMachine.EndpointInput
      { rateNume = rateNume,
        rateDeno = rateDeno,
        tokenAmount = tokenAmount
      }
  void $ Trace.waitNSlots 2

mintReserveCoins :: Integer -> Integer -> Integer -> Trace.EmulatorTrace ()
mintReserveCoins tokenAmount rateNume rateDeno = do
  hdl <- initialise
  Trace.callEndpoint @"mintReserveCoin"
    hdl
    CoinsMachine.EndpointInput
      { rateNume = rateNume,
        rateDeno = rateDeno,
        tokenAmount = tokenAmount
      }
  void $ Trace.waitNSlots 2


mintAndRedeemStableCoins :: Integer -> Integer -> Integer -> Integer -> Trace.EmulatorTrace ()
mintAndRedeemStableCoins tokenAmountToMint tokenAmountToRedeem rateNume rateDeno = do
  hdl <- initialise
  Trace.callEndpoint @"mintStableCoin"
    hdl
    CoinsMachine.EndpointInput
      { rateNume = rateNume,
        rateDeno = rateDeno,
        tokenAmount = tokenAmountToMint
      }
  void $ Trace.waitNSlots 2  
  Trace.callEndpoint @"redeemStableCoin"
      hdl
      CoinsMachine.EndpointInput
        { rateNume = rateNume,
          rateDeno = rateDeno,
          tokenAmount = tokenAmountToRedeem
        }
  void $ Trace.waitNSlots 2

mintAndRedeemReserveCoins :: Integer -> Integer -> Integer -> Integer -> Trace.EmulatorTrace ()
mintAndRedeemReserveCoins tokenAmountToMint tokenAmountToRedeem rateNume rateDeno = do
  hdl <- initialise
  Trace.callEndpoint @"mintReserveCoin"
    hdl
    CoinsMachine.EndpointInput
      { rateNume = rateNume,
        rateDeno = rateDeno,
        tokenAmount = tokenAmountToMint
      }
  void $ Trace.waitNSlots 2  
  Trace.callEndpoint @"redeemReserveCoin"
      hdl
      CoinsMachine.EndpointInput
        { rateNume = rateNume,
          rateDeno = rateDeno,
          tokenAmount = tokenAmountToRedeem
        }
  void $ Trace.waitNSlots 2
