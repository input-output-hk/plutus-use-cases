-- | Init blockchain state for tests
module Test.Nft.Init(
    checkOptions
  , w1, w2, w3
  , userAct1, userAct2, userAct3
  , adaCoin
  , initialDistribution
  , toUserId
  , nftId
  , nftContent
) where

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
import Mlabs.Nft.Logic.Types (UserAct(..), NftId, toNftId)
import qualified Mlabs.Nft.Contract.Nft as N

import Test.Utils (next)

checkOptions :: CheckOptions
checkOptions = defaultCheckOptions & emulatorConfig . Trace.initialChainState .~ Left initialDistribution

-- | Wallets that are used for testing.
w1, w2, w3 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3

toUserId :: Wallet -> UserId
toUserId = UserId . pubKeyHash . walletPubKey

-- | Showrtcuts for user actions
userAct1, userAct2, userAct3 :: UserAct -> Trace.EmulatorTrace ()
userAct1 act = N.callUserAct nftId w1 act >> next
userAct2 act = N.callUserAct nftId w2 act >> next
userAct3 act = N.callUserAct nftId w3 act >> next

nftId :: NftId
nftId = toNftId nftContent

nftContent :: ByteString
nftContent = "Mona Lisa"

-- | Initial distribution of wallets for testing
initialDistribution :: M.Map Wallet Value
initialDistribution = M.fromList
  [ (w1, val 1000)
  , (w2, val 1000)
  , (w3, val 1000)
  ]
  where
    val x = Value.singleton Ada.adaSymbol Ada.adaToken x

