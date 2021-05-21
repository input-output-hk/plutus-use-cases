--  |Transition function for NFTs
module Mlabs.Nft.Logic.React where

import Control.Monad.State.Strict (modify', gets)

import PlutusTx.Prelude

import Mlabs.Control.Check
import Mlabs.Emulator.Blockchain
import Mlabs.Lending.Logic.Types (adaCoin)
import Mlabs.Nft.Logic.State
import Mlabs.Nft.Logic.Types

{-# INLINABLE react #-}
react :: Act -> St [Resp]
react inp = do
  checkInputs inp
  case inp of
    Buy uid price newPrice -> buyAct uid price newPrice
    SetPrice uid price     -> setPriceAct uid price
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
checkInputs :: Act -> St ()
checkInputs = \case
  Buy _uid price newPrice -> do
    isPositive "Buy price" price
    mapM_ (isPositive "New price") newPrice

  SetPrice _uid price -> mapM_ (isPositive "Set price") price

