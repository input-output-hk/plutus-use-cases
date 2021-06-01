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

module Common.Schema where

import Rhyolite.Schema
import Data.Aeson
import Data.Text (Text)
import GHC.Generics


data PlaceHolder = PlaceHolder 
  { _placeHolder_placeholder :: Text
  } deriving (Eq, Show, Generic)

instance HasId PlaceHolder
instance FromJSON PlaceHolder
instance ToJSON PlaceHolder
