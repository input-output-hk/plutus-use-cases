{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Backend where

import Prelude hiding (id, (.))
import Backend.Notification
import Backend.RequestHandler
import Backend.Schema
import Backend.ViewSelector
import Common.Route
import Control.Category
import Control.Monad.Logger
import Control.Monad.IO.Class
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity
import Database.Groundhog (runMigration)
import Database.Groundhog.Generic.Migration hiding (migrateSchema)
import Database.Groundhog.Postgresql
import Obelisk.Backend
import Rhyolite.Backend.Account
import Rhyolite.Backend.App
import Rhyolite.Backend.DB
import Rhyolite.Backend.DB.Gargoyle
import Rhyolite.Backend.Logging
import qualified Web.ClientSession as CS

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      csk <- CS.getKey "clientSessionKey"
      withLogging [LoggingConfig (RhyoliteLogAppender_Stderr $ RhyoliteLogAppenderStderr $ Nothing) (Just mempty)] $ do
        logger <- fmap LoggingEnv askLoggerIO
        liftIO $ withDb "db" $ \db -> do
          _ <- runLoggingEnv logger $ do
            runDb (Identity db) $ do
              tableInfo <- getTableAnalysis
              runMigration $ do
                migrateAccount tableInfo
                migrateSchema tableInfo -- TODO: make sure migrations are handled for additional schemas
              addDefaultAccount
          (listen, _) <- serveDbOverWebsockets db
            (handleRequests logger csk db)
            (notifyHandler logger csk db)
            (viewSelectorHandler csk logger db)
            (queryMorphismPipeline $ transposeMonoidMap . monoidMapQueryMorphism)
          liftIO $ serve $ \case
            BackendRoute_Missing :=> _ -> return ()
            BackendRoute_Listen :=> Identity () -> listen

  , _backend_routeEncoder = backendRouteEncoder
  }

addDefaultAccount :: (PersistBackend m, SqlDb (PhantomDb m), MonadIO m) => m ()
addDefaultAccount = do
  (_,acc) <- ensureAccountExists Notification_Foo "foo@bar.com"
  setAccountPassword acc "asdf"

