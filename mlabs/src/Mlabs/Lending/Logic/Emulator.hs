-- | Simple emulation ob blockchain state
module Mlabs.Lending.Logic.Emulator(
    BchState(..)
  , BchWallet(..)
  , Resp(..)
  , Move(..)
  , applyResp
) where

import Data.Map.Strict (Map)
import Mlabs.Lending.Logic.Types

import qualified Data.Map.Strict as M

-- | Blockchain state is a set of wallets
newtype BchState = BchState (Map UserId BchWallet)

-- " For simplicity wallet is a map of coins to balances.
newtype BchWallet = BchWallet (Map Coin Integer)

-- | We can give money to vallets and take it from them
data Resp
  = MoveTo Move

-- | Moving funds
data Move = Move
  { move'addr   :: UserId    -- where move happens
  , move'coin   :: Coin      -- on which value
  , move'amount :: Integer   -- how many to add (can be negative)
  }

-- | Applies reponse to the blockchain state.
applyResp :: Resp -> BchState -> BchState
applyResp resp (BchState wallets) = BchState $ case resp of
  MoveTo act -> moveTo act wallets
  where
    moveTo Move{..} m = updateWallet move'addr move'coin move'amount m

    updateWallet addr coin amt m = M.update (Just . updateBalance coin amt) addr m
    updateBalance coin amt (BchWallet bals) = BchWallet $ M.update (\x -> Just (x + amt)) coin bals

