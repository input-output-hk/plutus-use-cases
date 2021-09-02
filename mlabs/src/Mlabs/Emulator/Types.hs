{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fobject-code #-}

module Mlabs.Emulator.Types (
  UserId (..),
  Coin,
  adaCoin,
  ownUserId,
) where

import PlutusTx.Prelude

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Plutus.Contract (AsContractError, Contract, ownPubKey)
import Plutus.V1.Ledger.Ada qualified as Ada
import Ledger.Contexts (pubKeyHash)
import Plutus.V1.Ledger.Crypto (PubKeyHash (..))
import Plutus.V1.Ledger.Value (AssetClass (..))
import PlutusTx (unstableMakeIsData)
import Prelude qualified as Hask

-- | Address of the wallet that can hold values of assets
data UserId
  = UserId PubKeyHash -- user address
  | Self -- addres of the lending platform
  deriving stock (Hask.Show, Generic, Hask.Eq, Hask.Ord)
  deriving anyclass (FromJSON, ToJSON)

instance Eq UserId where
  {-# INLINEABLE (==) #-}
  Self == Self = True
  UserId a == UserId b = a == b
  _ == _ = False

{-# INLINEABLE adaCoin #-}
adaCoin :: Coin
adaCoin = AssetClass (Ada.adaSymbol, Ada.adaToken)

-- | Custom currency
type Coin = AssetClass

PlutusTx.unstableMakeIsData ''UserId

-- | Get user id of the wallet owner.
ownUserId :: AsContractError e => Contract w s e UserId
ownUserId = fmap (UserId . pubKeyHash) ownPubKey
