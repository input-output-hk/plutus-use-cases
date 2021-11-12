{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Faucet.Data (AddressParam(..), TokenName(..), FaucetException(..)) where

import           Control.Exception (Exception)
import           Data.Aeson        (FromJSON, ToJSON)
import           Data.Typeable     (Typeable)
import           GHC.Generics      (Generic)
import           Servant           (FromHttpApiData)

newtype AddressParam = AddressParam { unAddress :: String }
  deriving (Eq, Show, Generic)
  deriving newtype (FromHttpApiData)

newtype TokenName = TokenName { unTokenName :: String }
  deriving (Eq, Show, Generic)
  deriving newtype (FromHttpApiData, ToJSON, FromJSON, Ord)

data FaucetException
  = SubmitTxError String
  | SomeError String
  | QueryProtocolParamsError String
  | QueryUtxoError String
  | UknownNetworkIdError String
  | AddressDecodingError String
  | SkeyDeserialiseError String
  | LackOfTokenNameError String
  | TokenNameNotSupportedError TokenName
  | NoUtxoToConsumeError
  deriving (Show, Typeable)

instance Exception FaucetException
