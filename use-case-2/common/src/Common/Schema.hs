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
import Data.Time.Clock
import Database.Beam
import Database.Beam.Backend.SQL.Types (SqlSerial)

data Db f = Db
  { _db_counter :: f (TableEntity CounterT)
  , _db_contracts :: f (TableEntity ContractT)
  , _db_pooledTokens :: f (TableEntity PooledTokenT)
  , _db_txFeeDataSet :: f (TableEntity TxFeeDataSetT)
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

data PooledTokenT f = PooledToken
  { _pooledToken_symbol :: Columnar f Text
  , _pooledToken_name :: Columnar f Text
  }
  deriving (Generic)

-- TODO: add timestamp
-- TODO: There may need to be optional fields of information depending on the smart contract action and parameters used.
  -- For example staking amounts, swap amounts, etc.
data TxFeeDataSetT f = TxFeeDataSet
  { _txFeeDataSet_id :: Columnar f (SqlSerial Int32)
  , _txFeeDataSet_txFee :: Columnar f Int32
  , _txFeeDataSet_smartContractAction :: Columnar f Text
  , _txFeeDataSet_estProcessingTime :: Columnar f Int32
  }
  deriving (Generic)

instance Beamable CounterT
instance Beamable ContractT
instance Beamable PooledTokenT
instance Beamable TxFeeDataSetT

instance Table CounterT where
  newtype PrimaryKey CounterT f = CounterId { _counterId_id :: Columnar f Int32 }
    deriving (Generic)
  primaryKey = CounterId . _counter_id

instance Table ContractT where
  newtype PrimaryKey ContractT f = ContractId { _contractId_id :: Columnar f Text }
    deriving (Generic)
  primaryKey = ContractId . _contract_id

instance Table PooledTokenT where
  data PrimaryKey PooledTokenT f = PooledTokenId { _pooledTokenId_symbol :: Columnar f Text, _pooledTokenId_name :: Columnar f Text }
    deriving (Generic)
  primaryKey pl = PooledTokenId (_pooledToken_symbol pl) (_pooledToken_name pl)

instance Table TxFeeDataSetT where
  data PrimaryKey TxFeeDataSetT f = TxFeeDataSetId { _txFeeDataId_id :: Columnar f (SqlSerial Int32)}
    deriving (Generic)
  primaryKey = TxFeeDataSetId . _txFeeDataSet_id

instance Beamable (PrimaryKey CounterT)
instance Beamable (PrimaryKey ContractT)
instance Beamable (PrimaryKey PooledTokenT)
instance Beamable (PrimaryKey TxFeeDataSetT)

type Counter = CounterT Identity
type Contract = ContractT Identity
type PooledToken = PooledTokenT Identity
type TxFeeData = TxFeeDataSetT Identity

deriving instance Show Counter
deriving instance Eq Counter
deriving instance FromJSON Counter
deriving instance ToJSON Counter

deriving instance Show Contract
deriving instance Eq Contract
deriving instance FromJSON Contract
deriving instance ToJSON Contract

deriving instance Show PooledToken
deriving instance Eq PooledToken
deriving instance Ord PooledToken
deriving instance FromJSON PooledToken
deriving instance ToJSON PooledToken

deriving instance Show TxFeeData
deriving instance Eq TxFeeData
deriving instance FromJSON TxFeeData
deriving instance ToJSON TxFeeData
