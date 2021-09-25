{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}

module Plutus.Abstract.RemoteData where

import qualified Control.Lens        as Lens
import qualified Control.Lens.Extras as Lens
import qualified Data.Aeson          as J
import           GHC.Generics        (Generic)
import           Prelude             hiding (maybe)
import qualified Test.QuickCheck     as Q

-- | A datatype representing fetched data.
-- |
-- | If you find yourself continually using `Maybe (Either e a)` to
-- | represent data loaded from an external source, or you have a
-- | habit of shuffling errors away to where they can be quietly
-- | ignored, consider using this. It makes it easier to represent the
-- | real state of a remote data fetch and handle it properly.
-- |
-- | For more on the motivation, take a look at the blog post
-- | [How Elm Slays A UI Antipattern](http://blog.jenkster.com/2016/06/how-elm-slays-a-ui-antipattern.html).
-- | This is a port of that original Elm module.
data RemoteData e a
  = NotAsked
  | Loading
  | Failure e
  | Success a
  deriving stock (Generic, Eq, Functor, Show, Foldable, Traversable)
  deriving anyclass (J.FromJSON, J.ToJSON)
-- TODO implement Applicative Monad Bifunctor MonadThrow MonadError Bifoldable Bitraversable

Lens.makeClassyPrisms ''RemoteData

instance Semigroup (RemoteData e a) where
  NotAsked <> x = x
  x <> NotAsked = x
  x <> y        = y

instance Monoid (RemoteData e a) where
  mempty = NotAsked

instance (Q.Arbitrary e, Q.Arbitrary a) => Q.Arbitrary (RemoteData e a) where
  arbitrary = do
    err <- Q.arbitrary
    res <- Q.arbitrary
    Q.elements [ NotAsked
               , Loading
               , Failure err
               , Success res]

------------------------------------------------------------

-- | Convert a `RemoteData` to a `Maybe`.
toMaybe :: forall e a. RemoteData e a -> Maybe a
toMaybe (Success value) = Just value
toMaybe _               = Nothing

-- | Convert a `Maybe` to `RemoteData`.
fromMaybe :: forall e a. Maybe a -> RemoteData e a
fromMaybe Nothing      = NotAsked
fromMaybe (Just value) = Success value

-- | Convert an `Either` to `RemoteData`
fromEither :: forall e a. Either e a -> RemoteData e a
fromEither (Left err)    = Failure err
fromEither (Right value) = Success value

-- | Takes a default value, a function, and a `RemoteData` value. If
-- | the data is `Success`, apply the function to the value, otherwise
-- | return the default.
-- |
-- | See also `withDefault`.
maybe :: forall e a b. b -> (a -> b) -> RemoteData e a -> b
maybe default' f (Success value) = f value
maybe default' f _               = default'

-- | If the `RemoteData` has been successfully loaded, return that,
-- | otherwise return a default value.
withDefault :: forall e a. a -> RemoteData e a -> a
withDefault default' = maybe default' id

------------------------------------------------------------

-- | Simple predicate.
isNotAsked :: forall e a. RemoteData e a -> Bool
isNotAsked = Lens.is _NotAsked

-- | Simple predicate.
isLoading :: forall e a. RemoteData e a -> Bool
isLoading = Lens.is _Loading

-- | Simple predicate.
isFailure :: forall e a. RemoteData e a -> Bool
isFailure = Lens.is _Failure

-- | Simple predicate.
isSuccess :: forall e a. RemoteData e a -> Bool
isSuccess = Lens.is _Success
