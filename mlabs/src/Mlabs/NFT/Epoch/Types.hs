module Mlabs.NFT.Epoch.Types 
  ( 
    UniqueAppToken(..),
    EpochAddress(..),
    EpochToken(..),
    EpochAct(..),
    EpochDatum(..),
  ) 
where

import Prelude qualified as Hask
import Mlabs.Data.LinkedList 
import PlutusTx.Prelude 
import Ledger
import PlutusTx qualified
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

-- | The single token that provides proof of uniqueness to the entire
-- application. The Token sits in the EUTXo containing the HEAD of the Epoch
-- list.
newtype UniqueAppToken = UniqueAppToken {uniqueapp'Token :: AssetClass}
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)
PlutusTx.unstableMakeIsData ''UniqueAppToken
PlutusTx.makeLift ''UniqueAppToken

newtype EpochAddress = EpochAddress { epoch'logic :: Address }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)
PlutusTx.unstableMakeIsData ''EpochAddress
PlutusTx.makeLift ''EpochAddress

-- | The Epoch Symbol is determined by the UniqueAppToken 
newtype EpochToken = EpochToken {et'assetClass :: AssetClass}
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)
PlutusTx.unstableMakeIsData ''EpochToken
PlutusTx.makeLift ''EpochToken

-- | Epoch Minting Policy Redeemer
data EpochAct
  = -- | Initialise a new epoch with a new EpochSymbol.
    EpochInit
  | -- | Forward the Epoch. Must make sure all of the polls are closed. 
    EpochNext
  | -- | Create the first Epoch Token.
    EpochCreateFirstEpoch
PlutusTx.makeLift ''EpochAct
PlutusTx.unstableMakeIsData ''EpochAct

-- | Datum information stored by the Epoch Head utxo. 
data EpochHead = EpochHead ()
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)
PlutusTx.makeLift ''EpochHead
PlutusTx.unstableMakeIsData ''EpochHead

-- | Datum information stored by a an Epoch Node utxo. 
data EpochNode = EpochNode
  { nftLogic'epoch :: Address
  , 'epoch :: Address
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)
PlutusTx.makeLift ''EpochNode
PlutusTx.unstableMakeIsData ''EpochNode

type EpochLList = LList Integer EpochHead EpochNode

newtype EpochDatum = EpochDatum { getNode'EpochList :: EpochLList }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)
PlutusTx.unstableMakeIsData ''EpochDatum
PlutusTx.makeLift ''EpochDatum
