{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Notification where

import Common.App
import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint.Extras.TH
import Data.GADT.Show.TH
import Data.Pool
import Database.Groundhog.Postgresql
import Rhyolite.Account
import Rhyolite.Backend.Listen
import Rhyolite.Backend.Logging
import Rhyolite.Schema
import qualified Web.ClientSession as CS

data Notification :: * -> * where
  Notification_Foo :: Notification (Id Account)

-- Notifies the frontend when a table in the database has undergone a transaction or change
notifyHandler :: forall a. Semigroup a => LoggingEnv -> CS.Key -> Pool Postgresql -> DbNotification Notification -> DefAppViewSelector a -> IO (DefAppView a)
notifyHandler _ _ _ = return mempty

-- Template Haskell Meta Programming functions to create and derive instances for the Notification type
deriveJSONGADT ''Notification
deriveArgDict ''Notification
deriveGShow ''Notification

