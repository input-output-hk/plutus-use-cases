{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Backend.Notification where

import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint.Extras.TH
import Data.GADT.Show.TH

import Common.Schema

data Notification :: * -> * where
  Notification_Contract :: Notification Contract

deriveJSONGADT ''Notification
deriveArgDict ''Notification
deriveGShow ''Notification

deriving instance Show (Notification a)
