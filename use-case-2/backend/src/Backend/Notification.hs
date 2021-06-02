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
import Data.Int

-- import Common.Schema

data Notification :: * -> * where
  Notification_Counter :: Notification Int32

deriveJSONGADT ''Notification
deriveArgDict ''Notification
deriveGShow ''Notification

deriving instance Show (Notification a)
