{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE EmptyCase #-}


module Common.App where

import Data.Aeson
import Data.Align 
import Data.Map.Monoidal
import qualified Data.Map.Monoidal as MMap
import Data.Text (Text)
import Data.Semigroup (First)
import GHC.Generics
import Reflex hiding (Request)
import Rhyolite.App
import Rhyolite.Request.TH
import Rhyolite.Schema

cookieKey :: Text
cookieKey = "authorizationCookie"

data DefApp = DefApp

instance HasRequest DefApp where
  data PublicRequest DefApp a where
    PublicRequest_Login :: Email -> Text -> PublicRequest DefApp (Either Text (AppCredential DefApp))
  data PrivateRequest DefApp a where
    PrivateRequest_Bar :: Text -> PrivateRequest DefApp (Either Text ())

data DefAppView a = DefAppView
  { _appDefView_echo :: MonoidalMap Text (a, First (Maybe Text))
  }
  deriving (Eq, Show, Functor, Foldable, Generic)

data DefAppViewSelector a = DefAppViewSelector
  { _appDefViewSelector_echo :: MonoidalMap Text a
  }
  deriving (Eq, Show, Functor, Foldable, Generic, Traversable)

instance HasView DefApp where
  type View DefApp = DefAppView
  type ViewSelector DefApp = DefAppViewSelector

instance Group (DefAppViewSelector SelectedCount) where
  negateG = fmap negateG

instance Additive (DefAppViewSelector SelectedCount)

instance (Semigroup a) => Monoid (DefAppViewSelector a) where
  mempty = nil
  mappend = salign

instance (Semigroup a) => Semigroup (DefAppViewSelector a) where
  (<>) = mappend

instance Align DefAppViewSelector where
  nil = DefAppViewSelector
    { _appDefViewSelector_echo = MMap.empty
    }
  alignWith f a b = DefAppViewSelector
    { _appDefViewSelector_echo = alignWith f (_appDefViewSelector_echo a) (_appDefViewSelector_echo b)
    }

instance FunctorMaybe DefAppViewSelector where
  fmapMaybe f a = DefAppViewSelector
    { _appDefViewSelector_echo = fmapMaybe f $ _appDefViewSelector_echo a
    }

instance FunctorMaybe DefAppView where
  fmapMaybe f a = DefAppView
    { _appDefView_echo = fmapMaybeFst f $ _appDefView_echo a
    }

instance (Semigroup a) => Monoid (DefAppView a) where
  mempty = DefAppView
    { _appDefView_echo = MMap.empty
    }
  mappend a b = DefAppView
    { _appDefView_echo = _appDefView_echo a <> _appDefView_echo b
    }

instance (Semigroup a) => Semigroup (DefAppView a) where
  (<>) = mappend

instance (Semigroup a) => Query (DefAppViewSelector a) where
  type QueryResult (DefAppViewSelector a) = DefAppView a
  crop vs v =
      DefAppView
        { _appDefView_echo = MMap.intersectionWith const (_appDefView_echo v) (_appDefViewSelector_echo vs)
        }

instance ToJSON a => ToJSON (DefAppView a)
instance FromJSON a => FromJSON (DefAppView a)
instance ToJSON a => ToJSON (DefAppViewSelector a)
instance FromJSON a => FromJSON (DefAppViewSelector a)

makeRequestForDataInstance ''PublicRequest ''DefApp
makeRequestForDataInstance ''PrivateRequest ''DefApp

