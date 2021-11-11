module Mlabs.Data.LinkedList (
  LList (..),
  LowBounded (..),
  nextNode,
  getNodeKey,
  canInsertAfter,
  pointNodeTo,
  head'info,
  head'next,
  node'key,
  node'info,
  node'next,
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
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''LList
PlutusTx.makeLift ''LList
makeLenses ''LList

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
pointNodeTo :: (LowBounded a, Ord a) => forall b c. LList a b c -> LList a b c -> Maybe (LList a b c)
nodeA `pointNodeTo` nodeB
  | nodeB `canInsertAfter` nodeA =
    case nodeA of
      HeadLList i _ -> Just $ HeadLList i $ Just (getNodeKey nodeB)
      NodeLList k i _ -> Just $ NodeLList k i $ Just (getNodeKey nodeB)
  | otherwise =
    Nothing
