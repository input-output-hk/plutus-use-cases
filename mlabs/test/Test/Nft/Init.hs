{-# LANGUAGE DataKinds #-}
-- | Init blockchain state for tests
module Test.Nft.Init(
    Script
  , runScript
  , checkOptions
  , w1, w2, w3
  , userAct
  , adaCoin
  , initialDistribution
  , toUserId
  , nftContent
) where

import Control.Monad.Reader

import Prelude

import Control.Lens

import PlutusTx.Prelude (ByteString)

import Plutus.V1.Ledger.Value (Value)
import qualified Plutus.V1.Ledger.Ada as Ada
import qualified Plutus.V1.Ledger.Value as Value
import Plutus.V1.Ledger.Contexts (pubKeyHash)
import qualified Data.Map as M

import Plutus.Contract.Test hiding (tx)
import qualified Plutus.Trace.Emulator as Trace

import Mlabs.Emulator.Types
import Mlabs.Nft.Logic.Types (UserAct(..), NftId)
import qualified Mlabs.Nft.Contract.Nft as N

import Test.Utils (next)

import Control.Monad.Freer
import Plutus.Trace.Effects.RunContract
import Plutus.Trace.Effects.Waiting
import Plutus.Trace.Effects.EmulatorControl
import Plutus.Trace.Effects.EmulatedWalletAPI
import Control.Monad.Freer.Extras.Log
import Control.Monad.Freer.Error
import Plutus.Trace.Emulator

import qualified Mlabs.Data.Ray as R

checkOptions :: CheckOptions
checkOptions = defaultCheckOptions & emulatorConfig . Trace.initialChainState .~ Left initialDistribution

-- | Wallets that are used for testing.
w1, w2, w3 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3

toUserId :: Wallet -> UserId
toUserId = UserId . pubKeyHash . walletPubKey

-- | Helper to run the scripts for NFT-contract
type ScriptM a = ReaderT NftId ( Eff '[RunContract, Waiting, EmulatorControl, EmulatedWalletAPI, LogMsg String, Error EmulatorRuntimeError]) a
type Script = ScriptM ()

-- | Script runner. It inits NFT by user 1 and provides nft id to all sequent
-- endpoint calls.
runScript :: Script -> Trace.EmulatorTrace ()
runScript script = do
  nftId <- N.callStartNft w1 $ N.StartParams
    { sp'content = nftContent
    , sp'share   = 1 R.% 10
    , sp'price   = Nothing
    }
  next
  runReaderT script nftId

-- | User action call.
userAct :: Wallet -> UserAct -> Script
userAct wal act = do
  nftId <- ask
  lift $ N.callUserAct nftId wal act >> next

-- | NFT content for testing.
nftContent :: ByteString
nftContent = "Mona Lisa"

-- | Initial distribution of wallets for testing.
-- We have 3 users. All of them get 1000 lovelace at the start.
initialDistribution :: M.Map Wallet Value
initialDistribution = M.fromList
  [ (w1, val 1000)
  , (w2, val 1000)
  , (w3, val 1000)
  ]
  where
    val x = Value.singleton Ada.adaSymbol Ada.adaToken x

