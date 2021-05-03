-- | State transitions for Lending app
module Mlabs.Lending.Logic.State(
    react
  , Error
  , Move(..)
  , Resp(..)
  , Wallet(..)
  , applyResp
  , BchState(..)
) where

import Prelude

import Control.Monad.State.Strict

import Data.Text
import Mlabs.Lending.Logic.Types

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Error = Text

type St = StateT LendingPool (Either Error)

react :: Act -> St [Resp]
react = \case
  LpAct     act -> lpAct act
  PriceAct  act -> priceAct act
  GovernAct act -> governAct act
  where
    lpAct = \case
      DepositAct{..}                    -> depositAct act'amount act'asset act'onBehalfOf
      BorrowAct{..}                     -> borrowAct  act'asset act'amount act'rate act'onBehalfOf
      RepayAct{..}                      -> repayAct   act'asset act'amount act'rate act'onBehalfOf
      SwapBorrowRateModelAct{..}        -> swapBorrowRateModelAct act'asset act'rate
      SetUserReserveAsCollateralAct{..} -> setUserReserveAsCollateralAct act'asset act'useAsCollateral
      WithdrawAct{..}                   -> withdrawAct act'to act'amount act'asset
      FlashLoanAct                      -> flashLoanAct
      LiquidationCallAct{..}            -> liquidationCallAct act'collateral act'debt act'user act'debtToCover act'receiveAToken

    depositAct _ _ _ = todo
    borrowAct _ _ _ _ = todo
    repayAct _ _ _ _ = todo
    swapBorrowRateModelAct _ _ = todo
    setUserReserveAsCollateralAct _ _ = todo
    withdrawAct _ _ _ = todo
    flashLoanAct = todo
    liquidationCallAct _ _ _ _ _ = todo

    priceAct = \case
      SetAssetPrice coin rate -> setAssetPrice coin rate
      SetOracleAddr coin addr -> setOracleAddr coin addr

    setAssetPrice _ _ = todo
    setOracleAddr _ _ = todo

    governAct = \case
      AddReserve coin val -> addReserve coin val

    addReserve _ _ = todo

    todo = return []

----------------------------------------------------
-- simple emulation ob blockchain state

-- | Blockchain state is a set of wallets
newtype BchState = BchState (Map Addr Wallet)

-- " For simplicity wallet is a map of coins to balances.
newtype Wallet = Wallet (Map Coin Integer)

-- | We can give money to vallets and take it from them
data Resp
  = MoveTo Move

-- | Moving funds
data Move = Move
  { move'addr   :: Addr      -- where move happens
  , move'coin   :: Coin      -- on which value
  , move'amount :: Integer   -- how many to add (can be negative)
  }

applyResp :: Resp -> BchState -> BchState
applyResp resp (BchState wallets) = BchState $ case resp of
  MoveTo act -> moveTo act wallets
  where
    moveTo Move{..}   m = updateWallet move'addr move'coin move'amount m

    updateWallet addr coin amt m = M.update (Just . updateBalance coin amt) addr m
    updateBalance coin amt (Wallet bals) = Wallet $ M.update (\x -> Just (x + amt)) coin bals

