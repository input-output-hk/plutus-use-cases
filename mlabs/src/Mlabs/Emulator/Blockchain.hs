{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fobject-code #-}

-- | Simple emulation ob blockchain state
module Mlabs.Emulator.Blockchain (
  BchState (..),
  BchWallet (..),
  defaultBchWallet,
  Resp (..),
  applyResp,
  moveFromTo,
  toConstraints,
  updateRespValue,
) where

import PlutusTx.Prelude hiding (fromMaybe, maybe)

import Data.Map.Strict as M (Map, alterF, empty, toList)
import Data.Maybe (fromMaybe, maybe)
import Ledger.Constraints (mustMintValue, mustPayToPubKey)
import Plutus.Contract.StateMachine (TxConstraints, Void)
import Plutus.V1.Ledger.Value (Value, assetClassValue)
import Prelude qualified as Hask (Eq, Show)

import Mlabs.Emulator.Types (Coin, UserId (..))

-- | Blockchain state is a set of wallets
newtype BchState = BchState (M.Map UserId BchWallet)

-- | For simplicity wallet is a map of coins to balances.
newtype BchWallet = BchWallet (Map Coin Integer)
  deriving newtype (Hask.Show, Hask.Eq)

instance Eq BchWallet where
  (BchWallet a) == (BchWallet b) = M.toList a == M.toList b

-- | Default empty wallet
defaultBchWallet :: BchWallet
defaultBchWallet = BchWallet M.empty

{- | We can give money to wallets and take it from them.
 We can mint new aToken coins on lending platform and burn it.
-}
data Resp
  = -- | move coins on wallet
    Move
      { move'addr :: UserId -- where move happens
      , move'coin :: Coin -- on which value
      , move'amount :: Integer -- how many to add (can be negative)
      }
  | -- | mint new coins for lending platform
    Mint
      { mint'coin :: Coin
      , mint'amount :: Integer
      }
  | -- | burns coins for lending platform
    Burn
      { mint'coin :: Coin
      , mint'amount :: Integer
      }
  deriving stock (Hask.Show)

-- | Moves from first user to second user
moveFromTo :: UserId -> UserId -> Coin -> Integer -> [Resp]
moveFromTo from to coin amount =
  [ Move from coin (negate amount)
  , Move to coin amount
  ]

-- | Applies response to the blockchain state.
applyResp :: Resp -> BchState -> Either BuiltinByteString BchState
applyResp resp (BchState wallets) = fmap BchState $ case resp of
  Move addr coin amount -> updateWallet addr coin amount wallets
  Mint coin amount -> updateWallet Self coin amount wallets
  Burn coin amount -> updateWallet Self coin (negate amount) wallets
  where
    updateWallet addr coin amt m = M.alterF (maybe (pure Nothing) (fmap Just . updateBalance coin amt)) addr m

    updateBalance :: Coin -> Integer -> BchWallet -> Either BuiltinByteString BchWallet
    updateBalance coin amt (BchWallet bals) = fmap BchWallet $ M.alterF (upd amt) coin bals

    upd amt x
      | res >= 0 = Right $ Just res
      | otherwise = Left "Negative balance"
      where
        res = fromMaybe 0 x + amt

---------------------------------------------------------------

{-# INLINEABLE toConstraints #-}
toConstraints :: Resp -> TxConstraints Void Void
toConstraints = \case
  Move addr coin amount | amount > 0 -> case addr of
    -- pays to lendex app
    Self -> mempty -- we already check this constraint with StateMachine
    -- pays to the user
    UserId pkh -> mustPayToPubKey pkh (assetClassValue coin amount)
  Mint coin amount -> mustMintValue (assetClassValue coin amount)
  Burn coin amount -> mustMintValue (assetClassValue coin $ negate amount)
  _ -> mempty

{-# INLINEABLE updateRespValue #-}
updateRespValue :: [Resp] -> Value -> Value
updateRespValue rs val = foldMap toRespValue rs <> val

{-# INLINEABLE toRespValue #-}
toRespValue :: Resp -> Value
toRespValue = \case
  Move Self coin amount -> assetClassValue coin amount
  Mint coin amount -> assetClassValue coin amount
  Burn coin amount -> assetClassValue coin (negate amount)
  _ -> mempty
