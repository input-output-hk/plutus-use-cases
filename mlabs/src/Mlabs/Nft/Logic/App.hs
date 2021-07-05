{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Application for testing NFT logic.
module Mlabs.Nft.Logic.App(
    NftApp
  , runNftApp
  , AppCfg(..)
  , defaultAppCfg
  --- * Script
  , Script
  , buy
  , setPrice
) where

import PlutusTx.Prelude
import Plutus.V1.Ledger.Crypto (PubKeyHash(..))
import Playground.Contract (TxOutRef(..))
import Plutus.V1.Ledger.TxId

import Mlabs.Emulator.App
import Mlabs.Emulator.Blockchain
import Mlabs.Emulator.Types
import qualified Mlabs.Emulator.Script as S

import Mlabs.Nft.Logic.React
import Mlabs.Nft.Logic.Types

import qualified Data.Map.Strict as M
import qualified Mlabs.Data.Ray as R

-- | NFT test emulator. We use it test the logic.
type NftApp = App Nft Act

-- | Config for NFT test emulator
data AppCfg = AppCfg
  { appCfg'users     :: [(UserId, BchWallet)] -- ^ state of blockchain
  , appCfg'nftInRef  :: TxOutRef
  , appCfg'nftData   :: ByteString            -- ^ nft content
  , appCfg'nftAuthor :: UserId                -- ^ author of nft
  }

-- | Run test emulator for NFT app.
runNftApp :: AppCfg -> Script -> NftApp
runNftApp cfg acts = runApp react (initApp cfg) acts

-- | Initialise NFT application.
initApp :: AppCfg -> NftApp
initApp AppCfg{..} = App
  { app'st      = initNft appCfg'nftInRef appCfg'nftAuthor appCfg'nftData (R.fromRational $ 1 % 10) Nothing
  , app'log     = []
  , app'wallets = BchState $ M.fromList $ (Self, defaultBchWallet) : appCfg'users
  }

-- | Default application.
-- It allocates three users each of them has 1000 ada coins.
-- The first user is author and the owner of NFT. NFT is locked with no price.
defaultAppCfg :: AppCfg
defaultAppCfg = AppCfg users dummyOutRef "mona-lisa" (fst $ users !! 0)
  where
    dummyOutRef = TxOutRef (TxId "") 0

    userNames = ["1", "2", "3"]

    users = fmap (\userName -> (UserId (PubKeyHash userName), wal (adaCoin, 1000))) userNames
    wal cs = BchWallet $ uncurry M.singleton cs

-------------------------------------------------------
-- script endpoints

type Script = S.Script Act

-- | User buys NFTs
buy :: UserId -> Integer -> Maybe Integer -> Script
buy uid price newPrice = S.putAct $ UserAct uid (BuyAct price newPrice)

-- | Set price of NFT
setPrice :: UserId -> Maybe Integer -> Script
setPrice uid newPrice = S.putAct $ UserAct uid (SetPriceAct newPrice)

