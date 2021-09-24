{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Common.Route where

import Prelude hiding (id, (.))

import Control.Category
import qualified Control.Categorical.Bifunctor as Cat
import Control.Monad.Except (MonadError)
import Data.Text (Text)
import Data.Functor.Identity

import Obelisk.Route
import Obelisk.Route.TH

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Listen :: BackendRoute ()
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRoute_ChooseWallet :: FrontendRoute ()
  FrontendRoute_WalletRoute :: FrontendRoute (Text, R WalletRoute)
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

data WalletRoute :: * -> * where
  WalletRoute_Swap :: WalletRoute ()
  WalletRoute_Portfolio :: WalletRoute ()
  WalletRoute_Pool :: WalletRoute ()

tupEncoder
  :: (MonadError Text check, check ~ parse)
  => Encoder check parse a p
  -> Encoder check parse c ([p], q)
  -> Encoder check parse (a, c) ([p], q)
tupEncoder e0 e1 = Cat.bimap e0 e1 >>> pathSegmentEncoder

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
    BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
    BackendRoute_Listen -> PathSegment "listen" $ unitEncoder mempty)
  (\case
    FrontendRoute_ChooseWallet -> PathEnd $ unitEncoder mempty
    FrontendRoute_WalletRoute -> PathSegment "wallet" $ tupEncoder id $ pathComponentEncoder $ \case
      WalletRoute_Swap -> PathSegment "swap" $ unitEncoder mempty
      WalletRoute_Portfolio -> PathSegment "portfolio" $ unitEncoder mempty
      WalletRoute_Pool -> PathSegment "pool" $ unitEncoder mempty)

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  , ''WalletRoute
  ]
