{-# LANGUAGE UndecidableInstances #-}

module Mlabs.Data.LinkedList (
  LList (..),
  LowBounded (..),
  nextNode,
  getNodeKey,
  canInsertAfter,
  pointNodeTo,
  pointNodeToMaybe,
  head'info,
  head'next,
  node'key,
  node'info,
  node'next,
  _HeadLList,
  _NodeLList,
) where

import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import PlutusTx qualified
import PlutusTx.Prelude
import Prelude qualified as Hask

-- | Class of Types that have a lower bound.
class LowBounded a where
  lowBound :: a

data LList key headInfo nodeInfo
  = HeadLList
      { _head'info :: headInfo
      , _head'next :: Maybe key
      }
  | NodeLList
      { _node'key :: key
      , _node'info :: nodeInfo
      , _node'next :: Maybe key
      }
  deriving stock (Hask.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''LList
PlutusTx.makeLift ''LList
makeLenses ''LList
makePrisms ''LList

instance (Eq k, Eq h, Eq n) => Eq (LList k h n) where
  {-# INLINEABLE (==) #-}
  (HeadLList a b) == (HeadLList a' b') = a == a' && b == b'
  (NodeLList a b c) == (NodeLList a' b' c') = a == a' && b == b' && c == c'
  _ == _ = False

instance (Eq (LList key headInfo nodeInfo), Ord key) => Ord (LList key headInfo nodeInfo) where
  {-# INLINEABLE (<=) #-}
  HeadLList {} <= NodeLList {} = True
  (NodeLList k1 _ _) <= (NodeLList k2 _ _) = k1 <= k2
  _ <= _ = False

instance Eq key => Hask.Eq (LList key headInfo nodeInfo) where
  (NodeLList k1 _ _) == (NodeLList k2 _ _) = k1 == k2
  _ == _ = False

instance (Hask.Eq (LList key headInfo nodeInfo), Hask.Ord key) => Hask.Ord (LList key headInfo nodeInfo) where
  HeadLList {} <= NodeLList {} = True
  (NodeLList k1 _ _) <= (NodeLList k2 _ _) = k1 Hask.<= k2
  _ <= _ = False

nextNode :: forall a b c. LList a b c -> Maybe a
nextNode = \case
  HeadLList _ n -> n
  NodeLList _ _ n -> n

-- | Node Key getter.
getNodeKey :: LowBounded a => forall b c. LList a b c -> a
getNodeKey = \case
  HeadLList _ _ -> lowBound
  NodeLList k _ _ -> k

{- | Utility function that checks if a node can be inserted after another list
 node.
-}
canInsertAfter :: (LowBounded a, Ord a) => forall b c. LList a b c -> LList a b c -> Bool
insertingNode `canInsertAfter` leftNode = insKey > leftKey && nextOk
  where
    insKey = getNodeKey insertingNode
    leftKey = getNodeKey leftNode
    nextOk = case nextNode leftNode of
      Nothing -> True
      Just k -> insKey < k

-- | Utility function that returns the updated List Node.
pointNodeTo :: (Ord a) => forall b c. LList a b c -> LList a b c -> Maybe (LList a b c)
pointNodeTo a b = pointNodeToMaybe a (Just b)

{- | Utility function that optionally points List Node to another List node,
 and returns the updated List Node.
-}
pointNodeToMaybe :: (Ord a) => forall b c. LList a b c -> Maybe (LList a b c) -> Maybe (LList a b c)
(HeadLList i _) `pointNodeToMaybe` Nothing = Just (HeadLList i Nothing)
(HeadLList i _) `pointNodeToMaybe` (Just (NodeLList k _ _)) = Just (HeadLList i (Just k))
(NodeLList k1 i _) `pointNodeToMaybe` Nothing = Just (NodeLList k1 i Nothing)
(NodeLList k1 i _) `pointNodeToMaybe` (Just (NodeLList k2 _ _)) =
  if k1 < k2
    then Just (NodeLList k1 i (Just k2))
    else Nothing
_ `pointNodeToMaybe` _ = Nothing
