{-# LANGUAGE ImportQualifiedPost #-}

module Mlabs.EfficientNFT.Marketplace (
  mkPolicy,
  MintAct (MintToken, ChangePric, ChangeOwner),
  OwnerData (..),
  PlatformConfig (..),
  policy,
) where

import Data.ByteString (ByteString)
import Ledger qualified
import Ledger.Crypto (PubKeyHash)
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Scripts qualified as Scripts
import PlutusTx.Natural (Natural)

-- TODO
mkValidator ::
  TxOutRef ->
  PubKeyHash ->
  Natural ->
  PlatformConfig ->
  BuiltinData ->
  BuiltinData ->
  ScriptContext ->
  Bool
mkValidator oref authorPkh royalty platformConfig mintAct ctx = False
