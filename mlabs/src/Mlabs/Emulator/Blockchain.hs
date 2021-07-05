{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fobject-code #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Simple emulation ob blockchain state
module Mlabs.Emulator.Blockchain(
    BchState(..)
  , BchWallet(..)
  , defaultBchWallet
  , Resp(..)
  , applyResp
  , moveFromTo
  , toConstraints
  , updateRespValue
) where

import qualified Prelude as P
import PlutusTx.Prelude hiding (fromMaybe, maybe)
import Plutus.V1.Ledger.Value (assetClassValue, Value)
import Ledger.Constraints

import Data.Maybe
import Data.Map.Strict (Map)
import Mlabs.Emulator.Types (Coin, UserId(..))

import qualified Data.Map.Strict as M
import qualified Plutus.Contract.StateMachine as SM

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

---------------------------------------------------------------

{-# INLINABLE toConstraints #-}
toConstraints :: Resp -> SM.TxConstraints SM.Void SM.Void
toConstraints = \case
  Move addr coin amount | amount > 0 -> case addr of
    -- pays to lendex app
    Self       -> mempty -- we already check this constraint with StateMachine
    -- pays to the user
    UserId pkh -> mustPayToPubKey pkh (assetClassValue coin amount)
  Mint coin amount      -> mustForgeValue (assetClassValue coin amount)
  Burn coin amount      -> mustForgeValue (assetClassValue coin $ negate amount)
  _ -> mempty

{-# INLINABLE updateRespValue #-}
updateRespValue :: [Resp] -> Value -> Value
updateRespValue rs val = foldMap toRespValue rs <> val

{-# INLINABLE toRespValue #-}
toRespValue :: Resp -> Value
toRespValue = \case
  Move Self coin amount -> assetClassValue coin amount
  Mint coin amount      -> assetClassValue coin amount
  Burn coin amount      -> assetClassValue coin (negate amount)
  _                     -> mempty

