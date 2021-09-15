-- | Definition of throwing and catching effects

module Middleware.Capability.Error
  ( AppError (..)
  , module E
  ) where

import Control.Exception as E hiding (catch, catchJust, fromException, throw, try, tryJust)
import Data.Aeson.Types  (FromJSON)
import Data.Text         (Text)
import Dex.WalletHistory (HistoryId)
import GHC.Generics
import Polysemy.Error    as E
import Servant.Client    (ClientError)

data AppError
  = PabError Text
  | ConfigLoaderError IOException
  | HttpError ClientError
  | OtherError Text
  | RetriesExceeded
  | CannotExtractHistoryId HistoryId
  | BodyParseError String
  | EndpointCallError Text
  deriving (Show)
