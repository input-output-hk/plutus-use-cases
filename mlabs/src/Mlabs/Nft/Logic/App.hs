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

import Mlabs.Emulator.App
import Mlabs.Emulator.Blockchain
import Mlabs.Emulator.Types
import qualified Mlabs.Emulator.Script as S

import Mlabs.Nft.Logic.React
import Mlabs.Nft.Logic.Types

import qualified Data.Map.Strict as M

-- | NFT test emulator. We use it test the logic.
type NftApp = App Nft Act

-- | Config for NFT test emulator
data AppCfg = AppCfg
  { appCfg'users     :: [(UserId, BchWallet)] -- ^ state of blockchain
  , appCfg'nftData   :: ByteString            -- ^ nft content
  , appCfg'nftAuthor :: UserId                -- ^ author of nft
  }

-- | Run test emulator for NFT app.
runNftApp :: AppCfg -> Script -> NftApp
runNftApp cfg acts = runApp react (initApp cfg) acts

-- | Initialise NFT application.
initApp :: AppCfg -> NftApp
initApp AppCfg{..} = App
  { app'st  = initNft appCfg'nftAuthor appCfg'nftData
  , app'log = []
  , app'wallets = BchState $ M.fromList $ (Self, defaultBchWallet) : appCfg'users
  }

initNft :: UserId -> ByteString -> Nft
initNft author content = Nft
  { nft'id     = toNftToken content
  , nft'data   = content
  , nft'share  = 1 % 10
  , nft'author = author
  , nft'owner  = author
  , nft'price  = Nothing
  }

-- | Default application.
-- It allocates three users each of them has 1000 ada coins.
-- The first user is author and the owner of NFT. NFT is locked with no price.
defaultAppCfg :: AppCfg
defaultAppCfg = AppCfg users "mona-lisa" (fst $ users !! 0)
  where
    userNames = ["1", "2", "3"]

    users = fmap (\userName -> (UserId (PubKeyHash userName), wal (adaCoin, 1000))) userNames
    wal cs = BchWallet $ uncurry M.singleton cs

-------------------------------------------------------
-- script endpoints

type Script = S.Script Act

-- | User buys NFTs
buy :: UserId -> Integer -> Maybe Integer -> Script
buy uid price newPrice = S.putAct $ Buy uid price newPrice

-- | Set price of NFT
setPrice :: UserId -> Maybe Integer -> Script
setPrice uid newPrice = S.putAct $ SetPrice uid newPrice

