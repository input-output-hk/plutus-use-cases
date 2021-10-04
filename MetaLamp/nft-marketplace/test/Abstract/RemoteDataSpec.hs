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

tests :: TestTree
tests =
  Q.testProperty "RemoteData Monoid instance" $
  Q.eq $ Q.prop_Monoid (Q.T :: Q.T (RemoteData String Int))
