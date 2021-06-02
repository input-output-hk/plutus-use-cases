{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |

module Common.Schema where

import Control.Monad.Identity
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Int
import Data.Text
import Database.Beam

data Db f = Db
  { _db_counter :: f (TableEntity CounterT)
  , _db_contracts :: f (TableEntity ContractT)
  }
  deriving (Generic, Database be)

data CounterT f = Counter
  { _counter_id :: Columnar f Int32
  , _counter_amount :: Columnar f Int32
  }
  deriving (Generic)

data ContractT f = Contract
  { _contract_id :: Columnar f Text
  , _contract_walletId :: Columnar f Int32
  }
  deriving (Generic)

instance Beamable CounterT
instance Beamable ContractT

instance Table CounterT where
  newtype PrimaryKey CounterT f = CounterId { _counterId_id :: Columnar f Int32 }
    deriving (Generic)
  primaryKey = CounterId . _counter_id

instance Table ContractT where
  newtype PrimaryKey ContractT f = ContractId { _contractId_id :: Columnar f Text }
    deriving (Generic)
  primaryKey = ContractId . _contract_id

instance Beamable (PrimaryKey CounterT)
instance Beamable (PrimaryKey ContractT)

type Counter = CounterT Identity
type Contract = ContractT Identity

deriving instance Show Counter
deriving instance Eq Counter
deriving instance FromJSON Counter
deriving instance ToJSON Counter

deriving instance Show Contract
deriving instance Eq Contract
deriving instance FromJSON Contract
deriving instance ToJSON Contract
