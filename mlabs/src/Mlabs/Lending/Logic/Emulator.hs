-- | Simple emulation ob blockchain state
module Mlabs.Lending.Logic.Emulator(
    BchState(..)
  , BchWallet(..)
  , defaultBchWallet
  , Resp(..)
  , applyResp
  , moveFromTo
) where

import qualified Prelude as P
import PlutusTx.Prelude hiding (fromMaybe, maybe)

import Data.Maybe
import Data.Map.Strict (Map)
import Mlabs.Lending.Logic.Types

import qualified Data.Map.Strict as M

-- | Blockchain state is a set of wallets
newtype BchState = BchState (Map UserId BchWallet)

-- " For simplicity wallet is a map of coins to balances.
newtype BchWallet = BchWallet (Map Coin Integer)
  deriving newtype (Show, P.Eq)

instance Eq BchWallet where
  (BchWallet a) == (BchWallet b) = M.toList a == M.toList b

-- | Default empty wallet
defaultBchWallet :: BchWallet
defaultBchWallet = BchWallet M.empty

-- | We can give money to vallets and take it from them.
-- We can mint new aToken coins on lending platform and burn it.
data Resp
  = Move
      { move'addr   :: UserId    -- where move happens
      , move'coin   :: Coin      -- on which value
      , move'amount :: Integer   -- how many to add (can be negative)
      }
  -- ^ move coins on wallet
  | Mint
      { mint'coin   :: Coin
      , mint'amount :: Integer
      }
  -- ^ mint new coins for lending platform
  | Burn
      { mint'coin   :: Coin
      , mint'amount :: Integer
      }
  -- ^ burns coins for lending platform
  deriving (Show)

-- | Moves from first user to second user
moveFromTo :: UserId -> UserId -> Coin -> Integer -> [Resp]
moveFromTo from to coin amount =
  [ Move from coin (negate amount)
  , Move to   coin amount
  ]

-- | Applies reponse to the blockchain state.
applyResp :: Resp -> BchState -> Either String BchState
applyResp resp (BchState wallets) = fmap BchState $ case resp of
  Move addr coin amount -> updateWallet addr coin amount wallets
  Mint coin amount      -> updateWallet Self coin amount wallets
  Burn coin amount      -> updateWallet Self coin (negate amount) wallets
  where
    updateWallet addr coin amt m = M.alterF (maybe (pure Nothing) (fmap Just . updateBalance coin amt)) addr m

    updateBalance :: Coin -> Integer -> BchWallet -> Either String BchWallet
    updateBalance coin amt (BchWallet bals) = fmap BchWallet $ M.alterF (upd amt) coin bals

    upd amt x
      | res >= 0  = Right $ Just res
      | otherwise = Left  $ "Negative balance"
      where
        res = fromMaybe 0 x + amt

