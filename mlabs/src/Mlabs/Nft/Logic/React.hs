{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fobject-code #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
-- | Transition function for NFTs
module Mlabs.Nft.Logic.React(react, checkInputs) where

import Control.Monad.State.Strict (modify', gets)

import PlutusTx.Prelude

import Mlabs.Control.Check (isPositive)
import Mlabs.Emulator.Blockchain (Resp(Move))
import Mlabs.Lending.Logic.Types (adaCoin)
import Mlabs.Nft.Logic.State (getAuthorShare, isOwner, isRightPrice, St)
import Mlabs.Nft.Logic.Types
    ( Act(..),
      Nft(nft'author, nft'owner, nft'price),
      UserAct(SetPriceAct, BuyAct) )

{-# INLINABLE react #-}
-- | State transitions for NFT contract logic.
react :: Act -> St [Resp]
react inp = do
  checkInputs inp
  case inp of
    UserAct uid (BuyAct price newPrice) -> buyAct uid price newPrice
    UserAct uid (SetPriceAct price)     -> setPriceAct uid price
  where
    -----------------------------------------------
    -- buy

    buyAct uid price newPrice = do
      isRightPrice price
      authorShare <- getAuthorShare price
      let total = authorShare + price
      author <- gets nft'author
      owner  <- gets nft'owner
      updateNftOnBuy
      pure
        [ Move uid    adaCoin (negate total)
        , Move owner  adaCoin price
        , Move author adaCoin authorShare
        ]
      where
        updateNftOnBuy =
          modify' $ \st -> st
            { nft'owner = uid
            , nft'price = newPrice
            }

    -----------------------------------------------
    -- set price

    setPriceAct uid price = do
      isOwner uid
      modify' $ \st -> st { nft'price = price }
      pure []

{-# INLINABLE checkInputs #-}
-- | Check inputs for valid values.
checkInputs :: Act -> St ()
checkInputs (UserAct _uid act) = case act of
  BuyAct price newPrice -> do
    isPositive "Buy price" price
    mapM_ (isPositive "New price") newPrice

  SetPriceAct price -> mapM_ (isPositive "Set price") price

