{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Application for testing NFT logic.
module Mlabs.Nft.Logic.App (
  NftApp,
  runNftApp,
  AppCfg (..),
  defaultAppCfg,
  --- * Script
  Script,
  buy,
  setPrice,
) where

import PlutusTx.Prelude
import Prelude qualified as Hask (uncurry)

import Data.Map.Strict qualified as M
import Playground.Contract (TxOutRef (..))
import Plutus.V1.Ledger.Crypto (PubKeyHash (..))
import Plutus.V1.Ledger.TxId (TxId (TxId))

import Mlabs.Emulator.App (App (..), runApp)
import Mlabs.Emulator.Blockchain (BchState (BchState), BchWallet (..), defaultBchWallet)
import Mlabs.Emulator.Script qualified as S
import Mlabs.Emulator.Types (UserId (..), adaCoin)
import Mlabs.Nft.Logic.React (react)
import Mlabs.Nft.Logic.Types (Act (..), Nft, UserAct (BuyAct, SetPriceAct), initNft)
import PlutusTx.Ratio qualified as R

-- | NFT test emulator. We use it test the logic.
type NftApp = App Nft Act

-- | Config for NFT test emulator
data AppCfg = AppCfg
  { -- | state of blockchain
    appCfg'users :: [(UserId, BchWallet)]
  , appCfg'nftInRef :: TxOutRef
  , -- | nft content
    appCfg'nftData :: BuiltinByteString
  , -- | author of nft
    appCfg'nftAuthor :: UserId
  }

-- | Run test emulator for NFT app.
runNftApp :: AppCfg -> Script -> NftApp
runNftApp cfg = runApp react (initApp cfg)

-- | Initialise NFT application.
initApp :: AppCfg -> NftApp
initApp AppCfg {..} =
  App
    { app'st = initNft appCfg'nftInRef appCfg'nftAuthor appCfg'nftData (1 R.% 10) Nothing
    , app'log = []
    , app'wallets = BchState $ M.fromList $ (Self, defaultBchWallet) : appCfg'users
    }

{- | Default application.
 It allocates three users each of them has 1000 ada coins.
 The first user is author and the owner of NFT. NFT is locked with no price.
-}
defaultAppCfg :: AppCfg
defaultAppCfg = AppCfg users dummyOutRef "mona-lisa" (fst $ users !! 0)
  where
    dummyOutRef = TxOutRef (TxId "") 0

    userNames = ["1", "2", "3"]

    users = fmap (\userName -> (UserId (PubKeyHash userName), wal (adaCoin, 1000))) userNames
    wal cs = BchWallet $ Hask.uncurry M.singleton cs

-------------------------------------------------------
-- script endpoints

type Script = S.Script Act

-- | User buys NFTs
buy :: UserId -> Integer -> Maybe Integer -> Script
buy uid price newPrice = S.putAct $ UserAct uid (BuyAct price newPrice)

-- | Set price of NFT
setPrice :: UserId -> Maybe Integer -> Script
setPrice uid newPrice = S.putAct $ UserAct uid (SetPriceAct newPrice)
