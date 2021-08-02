{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fobject-code #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | State transitions for NFT app
module Mlabs.Nft.Logic.State(
    St
  , isOwner
  , isRightPrice
  , getAuthorShare
) where

import PlutusTx.Prelude

import Mlabs.Control.Monad.State ( guardError, gets, PlutusState )
import qualified PlutusTx.Ratio as R
import Mlabs.Lending.Logic.Types (UserId)
import Mlabs.Nft.Logic.Types (Nft(nft'owner, nft'price, nft'share))



-- | State update of NFT
type St = PlutusState Nft

-----------------------------------------------------------
-- common functions

{-# INLINABLE isOwner #-}
-- | Check if user is owner of NFT
isOwner :: UserId -> St ()
isOwner uid = do
  owner <- gets nft'owner
  guardError "Not an owner" $ uid == owner

{-# INLINABLE isRightPrice #-}
-- | Check if price is enough to buy NFT
isRightPrice :: Integer -> St ()
isRightPrice inputPrice = do
  isOk <- any (inputPrice >= ) <$> gets nft'price
  guardError "Price not enough" isOk

{-# INLINABLE getAuthorShare #-}
-- | Get original author's share of the price of NFT
getAuthorShare :: Integer -> St Integer
getAuthorShare price = do
  share <- gets nft'share
  pure $ R.round $ R.fromInteger price * share

