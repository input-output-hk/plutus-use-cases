{-# LANGUAGE OverloadedStrings #-}

module Backend.ViewSelector where

import Common.App
import qualified Data.Map.Monoidal as MMap
import Data.Functor.Identity
import Data.Pool
import Data.Semigroup
import Database.Groundhog.Postgresql
import Rhyolite.Backend.App
import Rhyolite.Backend.DB
import Rhyolite.Backend.Logging
import qualified Web.ClientSession as CS

-- Queries database for requested data and returns it to the frontend view
viewSelectorHandler
  :: CS.Key
  -> LoggingEnv
  -> Pool Postgresql
  -> QueryHandler (DefAppViewSelector a) IO
viewSelectorHandler _csk logger db = QueryHandler $ \vs -> runLoggingEnv logger $ runDb (Identity db) $
  return DefAppView
    { _appDefView_echo = MMap.mapWithKey (\_ v -> (v, First (Just ""))) $ _appDefViewSelector_echo vs
    }

