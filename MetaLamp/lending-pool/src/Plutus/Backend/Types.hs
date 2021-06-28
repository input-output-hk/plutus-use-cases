{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}

module Plutus.Backend.Types where

import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Data.Aeson (FromJSON, ToJSON)
import Wallet.Emulator.Types (Wallet)

data Operation = Deposit | Withdrawn deriving (Show, Eq, Generic, FromJSON, ToJSON)
data Status = SuccessStatus | FailStatus T.Text deriving (Show, Eq, Generic, FromJSON, ToJSON)

data OperationStatus = OperationStatus {
    operation :: Operation,
    status :: Status
} deriving (Show, Eq, Generic, FromJSON, ToJSON)

data DepositRequest = DepositRequest {
    wallet :: Wallet,
    amount :: Integer
} deriving (Show, Eq, Generic, FromJSON, ToJSON)
