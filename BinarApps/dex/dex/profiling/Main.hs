{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}

module Main
  where

import qualified Cardano.Api                   as C
import           Data.Default
import           Data.Functor                  (void)
import qualified Data.Map                      as Map
import           Dex.OffChain
import           Dex.Trace                     (customTraceConfig)
import           Dex.Types
import           Ledger.Index                  (ValidatorMode (..))
import           Plutus.Trace.Emulator         as Emulator
import           Plutus.Trace.Emulator.Extract (Command (..),
                                                ScriptsConfig (..),
                                                writeScriptsTo)
import qualified Plutus.V1.Ledger.Ada          as Ada
import qualified Plutus.V1.Ledger.Value        as Value
import           Wallet.Emulator.Wallet        as Wallet

customSymbol :: [Char]
customSymbol = "ff"

customToken :: [Char]
customToken = "PLN"

customSymbolsAndTokens :: [(Value.CurrencySymbol, Value.TokenName)]
customSymbolsAndTokens = [("ff", "coin1"), ("ee", "coin2"), ("dd", "coin3"), ("cc", "coin4"), ("bb", "coin5")]


customSymbol2 :: [Char]
customSymbol2 = "ee"

customToken2 :: [Char]
customToken2 = "BTC"

customSymbol3 :: [Char]
customSymbol3 = "dd"

customToken3 :: [Char]
customToken3 = "ETH"

main' :: IO ()
main' = runTrace dexTrace

emulatorCfg :: EmulatorConfig
emulatorCfg = EmulatorConfig (Left $ Map.fromList ([(knownWallet i, v) | i <- [1 .. 4]])) def def
      where
        v = Ada.lovelaceValueOf 100_000_000 <> mconcat (map (\(symbol,tokenName) -> Value.singleton symbol tokenName 100_000_000) customSymbolsAndTokens)

scriptsConfig1 :: ScriptsConfig
scriptsConfig1 = ScriptsConfig "./" (Scripts FullyAppliedValidators  )

scriptsConfig2 :: ScriptsConfig
scriptsConfig2 = ScriptsConfig "./" (Scripts UnappliedValidators  )

scriptsConfig3 :: ScriptsConfig
scriptsConfig3 = ScriptsConfig "./" (Transactions (C.Testnet $ C.NetworkMagic 0) "protocol-parameters.json")


main :: IO ()
main = do
  r <- writeScriptsTo scriptsConfig1 "sell-order" dexTrace emulatorCfg
  print r
  return ()

runTrace :: EmulatorTrace () -> IO ()
runTrace = runEmulatorTraceIO' customTraceConfig emulatorCfg

dexTrace :: EmulatorTrace ()
dexTrace = do
  h1 <- activateContractWallet (knownWallet 1) dexEndpoints
  h2 <- activateContractWallet (knownWallet 2) dexEndpoints
  -- void $ callEndpoint @"createLiquidityPool" h1 (Request "a" 0 (LiquidityPoolParams (Value.AssetClass ("ff", "coin1")) (Value.AssetClass ("ee", "coin2")) 1000 (PriceChangeParams (5,100) (10,100) 2) (3,100) (1,2)))
  -- void $ waitNSlots 10

  void $ callEndpoint @"createLiquidityOrder" h1 (Request "b" 1 (LiquidityOrderParams (Value.AssetClass ("ff", "coin1")) (Value.AssetClass ("ee", "coin2")) 200 400 (1,100)))
  void $ waitNSlots 2
  void $ callEndpoint @"createLiquidityOrder" h1 (Request "b" 1 (LiquidityOrderParams (Value.AssetClass ("ff", "coin1")) (Value.AssetClass ("ee", "coin2")) 200 400 (1,100)))
  void $ waitNSlots 2

  void $ callEndpoint @"createLiquidityOrder" h1 (Request "b" 1 (LiquidityOrderParams (Value.AssetClass ("ff", "coin1")) (Value.AssetClass ("ee", "coin2")) 200 400 (1,100)))
  void $ waitNSlots 2
  -- void $ callEndpoint @"createSellOrder" h1 (Request "c" 2 (SellOrderParams (Value.AssetClass ("ff", "coin1")) (Value.AssetClass ("ee", "coin2")) 5 600))
  -- void $ waitNSlots 2
  -- void $ callEndpoint @"createSellOrder" h2 (Request "d" 3 (SellOrderParams (Value.AssetClass ("ee", "coin2")) (Value.AssetClass ("ff", "coin1")) 650 200))
  -- void $ waitNSlots 2
  -- void $ callEndpoint @"createSellOrder" h2 (Request "d" 4 (SellOrderParams (Value.AssetClass ("ee", "coin2")) (Value.AssetClass ("ff", "coin1")) 650 200))
  -- void $ waitNSlots 2

  void $ callEndpoint @"perform" h2 (Request "c" 5 ())
  void $ waitNSlots 2
  -- void $ callEndpoint @"perform" h2 (Request "c" 4 ())
  -- void $ waitNSlots 10
  -- void $ callEndpoint @"perform" h2 (Request "c" 4 ())
  -- void $ waitNSlots 10
  -- void $ callEndpoint @"collectFunds" h1 (Request "d" 5 ())
  -- void $ waitNSlots 10
