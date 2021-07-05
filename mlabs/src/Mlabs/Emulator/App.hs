{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Lending app emulator
module Mlabs.Emulator.App(
    App(..)
  , runApp
  , lookupAppWallet
  , noErrors
  , someErrors
  , checkWallets
) where

import Test.Tasty.HUnit
import Text.Show.Pretty

import PlutusTx.Prelude
import Control.Monad.State.Strict hiding (Functor(..))

import Data.List (foldl')

import Mlabs.Emulator.Blockchain
import Mlabs.Emulator.Script
import Mlabs.Emulator.Types

import Mlabs.Control.Monad.State

import qualified Data.Map.Strict as M

-- | Prototype application
data App st act = App
  { app'st      :: !st                   -- ^ lending pool
  , app'log     :: ![(act, st, String)]  -- ^ error log
                                         -- ^ it reports on which act and pool state error has happened
  , app'wallets :: !BchState             -- ^ current state of blockchain
  }

-- | Lookup state of the blockchain-wallet for a given user-id.
lookupAppWallet :: UserId -> App st act -> Maybe BchWallet
lookupAppWallet uid App{..} = case app'wallets of
  BchState wals -> M.lookup uid wals

-- | Runs application with the list of actions.
-- Returns final state of the application.
runApp :: (act -> PlutusState st [Resp]) -> App st act -> Script act -> App st act
runApp react app acts = foldl' go app (runScript acts)
  where
    -- There are two possible sources of errors:
    --   * we can not make transition to state (react produces Left)
    --   * the transition produces action on blockchain that leads to negative balances (applyResp produces Left)
    go (App lp errs wallets) act = case runStateT (react act) lp of
      Right (resp, nextState) -> case foldM (flip applyResp) wallets resp of
        Right nextWallets -> App nextState errs nextWallets
        Left err          -> App lp ((act, lp, err) : errs) wallets
      Left err                -> App lp ((act, lp, err) : errs) wallets


---------------------------------------------------
-- test functions

noErrors :: (Show act, Show st) => App st act -> Assertion
noErrors app = case app'log app of
  [] -> assertBool "no errors" True
  xs -> do
    mapM_ printLog xs
    assertFailure "There are errors"
  where
    printLog (act, lp, msg) = do
      pPrint act
      pPrint lp
      print msg

someErrors :: App st act -> Assertion
someErrors app = assertBool "Script fails" $ not $ null (app.app'log)

-- | Check that we have those wallets after script was run.
checkWallets :: (Show act, Show st) => [(UserId, BchWallet)] -> App st act -> Assertion
checkWallets wals app = mapM_ (uncurry $ hasWallet app) wals

-- | Checks that application state contains concrete wallet for a given user id.
hasWallet :: App st act -> UserId -> BchWallet -> Assertion
hasWallet app uid wal = lookupAppWallet uid app @=? Just wal

