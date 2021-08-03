{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module Ext.PlutusTx.AssocMap where

import           GHC.Generics      (Generic)
import           PlutusTx.AssocMap
import           PlutusTx.IsData
import           PlutusTx.Lift     (makeLift)
import           PlutusTx.Prelude  hiding (all, null, toList)
import qualified PlutusTx.Prelude  as P
import           PlutusTx.These
import qualified Prelude           as Haskell

{-# INLINABLE unionWith #-}
-- | Combine two 'Map's with the given combination function.
unionWith ::
     forall k a. (Eq k)
  => (a -> a -> a)
  -> Map k a
  -> Map k a
  -> Map k a
unionWith merge mls mrs =
  let ls = toList mls
      rs = toList mrs
      f :: a -> Maybe a -> a
      f a b' =
        case b' of
          Nothing -> a
          Just b  -> merge a b
      ls' :: [(k, a)]
      ls' = P.fmap (\(c, i) -> (c, f i (lookup c (fromList rs)))) ls
      rs' :: [(k, a)]
      rs' = filter (\(c, _) -> not (any (\(c', _) -> c' == c) ls)) rs
   in fromList (ls' ++ rs')
