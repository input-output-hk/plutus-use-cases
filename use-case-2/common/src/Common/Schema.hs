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
import Database.Beam.Backend.SQL.Types (SqlSerial)

data Db f = Db
  { _db_contracts :: f (TableEntity ContractT)
  , _db_pooledTokens :: f (TableEntity PooledTokenT)
  , _db_txFeeDataSet :: f (TableEntity TxFeeDataSetT)
  }
  deriving (Generic, Database be)

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

-- Possible Improvements: There could be optional fields of information depending on
-- which smart contract action and parameters are used in order to improve regression
-- accuracy. For example staking amounts, swap amounts, etc.
data TxFeeDataSetT f = TxFeeDataSet
  { _txFeeDataSet_id :: Columnar f (SqlSerial Int32)
  , _txFeeDataSet_txFee :: Columnar f Int32
  , _txFeeDataSet_smartContractAction :: Columnar f Text
  , _txFeeDataSet_estProcessingTime :: Columnar f Int32
  , _txFeeDataSet_scriptSize :: Columnar f Int32
  }
  deriving (Generic)

instance Beamable ContractT
instance Beamable PooledTokenT
instance Beamable TxFeeDataSetT

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

instance Beamable (PrimaryKey ContractT)
instance Beamable (PrimaryKey PooledTokenT)
instance Beamable (PrimaryKey TxFeeDataSetT)

type Contract = ContractT Identity
type PooledToken = PooledTokenT Identity
type TxFeeData = TxFeeDataSetT Identity

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
