{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | State transitions for Lending app
module Mlabs.Nft.Logic.State(
    St
  , Error
  , isOwner
  , isRightPrice
  , getAuthorShare
  , isPositive
) where

import qualified PlutusTx.Ratio as R
import qualified PlutusTx.Numeric as N
import PlutusTx.Prelude
import PlutusTx.AssocMap (Map)
import qualified PlutusTx.AssocMap as M

import Control.Monad.Except       hiding (Functor(..), mapM)
import Control.Monad.State.Strict hiding (Functor(..), mapM)

import Mlabs.Lending.Logic.State (guardError, isPositive)
import Mlabs.Lending.Logic.Types (UserId(..))
import Mlabs.Nft.Logic.Types

-- | Type for errors
type Error = String

-- | State update of lending pool
type St = StateT Nft (Either Error)

instance Functor St where
  {-# INLINABLE fmap #-}
  fmap f (StateT a) = StateT $ fmap (\(v, st) -> (f v, st)) . a

instance Applicative St where
  {-# INLINABLE pure #-}
  pure a = StateT (\st -> Right (a, st))

  {-# INLINABLE (<*>) #-}
  (StateT f) <*> (StateT a) = StateT $ \st -> case f st of
    Left err -> Left err
    Right (f1, st1) -> fmap (\(a1, st2) -> (f1 a1, st2)) $ a st1

-----------------------------------------------------------
-- common functions

{-# INLINABLE isOwner #-}
isOwner :: UserId -> St ()
isOwner uid = do
  owner <- gets nft'owner
  guardError "Not an owner" $ uid == owner

{-# INLINABLE isRightPrice #-}
isRightPrice :: Integer -> St ()
isRightPrice inputPrice = do
  cond <- maybe False (inputPrice >= ) <$> gets nft'price
  guardError "Price not enough" cond


{-# INLINABLE getAuthorShare #-}
getAuthorShare :: Integer -> St Integer
getAuthorShare price = do
  share <- gets nft'share
  pure $ R.round $ R.fromInteger price * share

