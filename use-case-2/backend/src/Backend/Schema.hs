{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Backend.Schema where

import Common.Schema
import Database.Groundhog.TH
-- import Database.Id.Groundhog.TH
import Rhyolite.Backend.Account ()
import Rhyolite.Backend.Schema.TH

mkRhyolitePersist (Just "migrateSchema") [groundhog| 
  - entity: PlaceHolder
|]

fmap concat $ mapM (uncurry makeDefaultKeyIdInt64)
  [
    (''PlaceHolder, 'PlaceHolderKey)
  ]
