{-# LANGUAGE OverloadedStrings #-}

module Middleware.Capability.Retry where

import Colog.Polysemy.Formatting         (logError, logInfo)
import Colog.Polysemy.Formatting.WithLog (WithLog)
import Control.DeepSeq                   (NFData)
import Data.Either.Combinators           (mapLeft)
import Formatting
import Middleware.Capability.Error       (AppError (HttpError, OtherError, RetriesExceeded), Error, catch,
                                          throw)
import Middleware.Capability.Time        (Time, sleep)
import Middleware.PabClient.Types        (lookupResBody)
import Polysemy                          (Members, Sem)
import Servant.Client.Streaming          (ClientError, ClientM)
import Servant.Polysemy.Client           (ClientError, ServantClient, runClient, runClient')
import Text.ParserCombinators.ReadP      (satisfy)

-- | retry servant request with custom response mapping
retryRequest :: (NFData a, WithLog r, Members '[ServantClient, Error AppError, Time] r)
             => Integer
             -- ^ limit of retries
             -> Integer
             -- ^ interval seconds
             -> (a -> Either AppError b)
             -- ^ try to map response body
             -> ClientM a
             -- ^ retryable servant call
             -> Sem r b
retryRequest retriesLeft _ action _ | retriesLeft <= 0 = do
  logError "Giving Up"
  throw RetriesExceeded
retryRequest retriesLeft interval f action = do
  callRes <- runClient' action
  let res = f =<< mapLeft HttpError callRes
  case res of
    Right o  -> pure o
    Left err -> do
      logError (text % shown) "An error occurred: " err
      sleep (seconds interval)
      logInfo ("An error occurred while performing request. Retries left: " % accessed id int) retriesLeft
      retryRequest (retriesLeft - 1) interval f action
  where
    seconds = (*) 100000
