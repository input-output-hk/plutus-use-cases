{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Abstract.ContractRespSpec
  ( tests
  ) where

import           Plutus.Abstract.ContractResp      (ContractResp (..))
import qualified Test.QuickCheck.Property.Common as Q
import qualified Test.QuickCheck.Property.Monoid as Q
import           Test.Tasty
import qualified Test.Tasty.QuickCheck           as Q

tests :: TestTree
tests =
  Q.testProperty "ContractResp Monoid instance" $
  Q.eq $ Q.prop_Monoid (Q.T :: Q.T (ContractResp String))
