{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Main 
  (

  ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Freer.Extras.Log     (LogLevel (..))
import           Data.Maybe
import           Test.QuickCheck                    as QC hiding ((.&&.))
import           Test.Tasty                         hiding (after)
import qualified Test.Tasty.HUnit                   as HUnit
import           Test.Tasty.QuickCheck              (testProperty)

import           Plutus.Contract.Test
import qualified Ledger.Ada                         as Ada
import           Ledger.Value
import           Plutus.Contract.Test               hiding (not)
import           Plutus.Contract.Test.ContractModel
import           Plutus.Contracts.Uniswap           as U
import           Plutus.Contracts.Validators        as V
import qualified Ledger.Typed.Scripts               as Scripts
import           Plutus.Trace.Emulator              as Trace

main :: IO ()
main = putStrLn "testing area, coming soon..."

w1, w2, w3 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3
w3 = Wallet 4

wallets :: [Wallet]
wallets = [w1, w2, w3, w4]

uniswapTokenVal :: Value
uniswapTokenVal =
    let sym = Scripts.monetaryPolicyHash V.uniswapInstance
    in G.token sym "guess"

-- token :: MonetaryPolicyHash -> TokenName -> Value
-- token mps tn = V.singleton (V.mpsSymbol mps) tn 1