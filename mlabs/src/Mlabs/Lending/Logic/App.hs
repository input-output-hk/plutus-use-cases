-- | Ann lending app emulator
module Mlabs.Lending.Logic.App(
    App(..)
  , runApp
  , initApp
) where

import Prelude

import Control.Monad.State.Strict
import Control.Arrow (second)

import Data.List (foldl')

import Mlabs.Lending.Logic.Emulator
import Mlabs.Lending.Logic.React
import Mlabs.Lending.Logic.Types
import Mlabs.Lending.Logic.State

import qualified Data.Map.Strict as M

data App = App
  { app'pool    :: !LendingPool
  , app'log     :: ![Error]
  , app'wallets :: !BchState
  }

runApp :: App -> [Act] -> App
runApp app acts = foldl' go app acts
  where
    go (App lp errs wallets) act = case runStateT (react act) lp of
      Right (resp, nextState) -> App nextState errs (foldl' (flip applyResp) wallets resp)
      Left err                -> App lp (err : errs) wallets

-- | App is initialised with list of coins and their rates (value relative to base currency, ada for us)
initApp :: [(Coin, Rational)] -> App
initApp coins = App
  { app'pool = LendingPool (M.fromList (fmap (second initReserve) coins)) mempty
  , app'log  = []
  , app'wallets = BchState $ M.fromList [(Self, defaultBchWallet)]
  }

