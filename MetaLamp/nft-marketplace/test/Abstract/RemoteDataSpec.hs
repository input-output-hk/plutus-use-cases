{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Abstract.RemoteDataSpec
  ( tests
  ) where

import           Plutus.Abstract.RemoteData      (RemoteData (..))
import qualified Test.QuickCheck.Property.Common as Q
import qualified Test.QuickCheck.Property.Monoid as Q
import           Test.Tasty
import qualified Test.Tasty.QuickCheck           as Q

-- warning: Orphan instance
instance (Q.Arbitrary e, Q.Arbitrary a) => Q.Arbitrary (RemoteData e a) where
  arbitrary = do
    err <- Q.arbitrary
    res <- Q.arbitrary
    Q.elements [ NotAsked
               , Loading
               , Failure err
               , Success res]

tests :: TestTree
tests =
  Q.testProperty "RemoteData Monoid instance" $
  Q.eq $ Q.prop_Monoid (Q.T :: Q.T (RemoteData String Int))
