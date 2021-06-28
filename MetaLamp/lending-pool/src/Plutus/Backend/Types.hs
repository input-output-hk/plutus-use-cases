{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Plutus.Backend.Types where

import           Data.Aeson            (FromJSON, ToJSON)
import qualified Data.Text             as T
import           GHC.Generics          (Generic)
import           Wallet.Emulator.Types (Wallet)

data Operation = Deposit | Withdrawn deriving (Show, Eq, Generic, FromJSON, ToJSON)
data OperationStatus = SuccessOperation | FailOperation String deriving (Show, Eq, Generic, FromJSON, ToJSON)

data DepositRequest = DepositRequest {
    wallet :: Wallet,
    amount :: Integer
} deriving (Show, Eq, Generic, FromJSON, ToJSON)
