-- | Init blockchain state for tests
module Test.Lending.Init(
    checkOptions
  , wAdmin, w1, w2, w3
  , userAct1, userAct2, userAct3
  , adaCoin, coin1, coin2, coin3
  , aAda, aToken1, aToken2, aToken3
  , aCoin1, aCoin2, aCoin3
  , initialDistribution
  , toUserId
  , toPubKeyHash
  , lendexId
  , fromToken
) where

import Prelude

import Control.Lens

import Plutus.V1.Ledger.Value (Value, TokenName)
import qualified Plutus.V1.Ledger.Ada as Ada
import qualified Plutus.V1.Ledger.Value as Value
import Plutus.V1.Ledger.Crypto (PubKeyHash(..))
import Plutus.V1.Ledger.Contexts (pubKeyHash)
import qualified Data.Map as M

import Plutus.Contract.Test hiding (tx)
import qualified Plutus.Trace.Emulator as Trace

import Mlabs.Lending.Logic.Types (LendexId(..), Coin, UserAct(..), UserId(..))
import qualified Mlabs.Lending.Logic.App as L
import qualified Mlabs.Lending.Contract.Emulator.Client as L
import qualified Mlabs.Lending.Contract.Forge as Forge

checkOptions :: CheckOptions
checkOptions = defaultCheckOptions & emulatorConfig . Trace.initialChainState .~ Left initialDistribution

-- | Wallets that are used for testing.
wAdmin, w1, w2, w3 :: Wallet
wAdmin = Wallet 50
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3

toUserId :: Wallet -> UserId
toUserId = UserId . pubKeyHash . walletPubKey

toPubKeyHash :: Wallet -> PubKeyHash
toPubKeyHash = pubKeyHash . walletPubKey

-- | Identifier for our lendex platform
lendexId :: LendexId
lendexId = LendexId "MLabs lending platform"

-- | Showrtcuts for user actions
userAct1, userAct2, userAct3 :: UserAct -> Trace.EmulatorTrace ()
userAct1 = L.callUserAct lendexId w1
userAct2 = L.callUserAct lendexId w2
userAct3 = L.callUserAct lendexId w3

-- | Coins which are used for testing
adaCoin, coin1, coin2, coin3 :: Coin
coin1 = L.toCoin "Dollar"
coin2 = L.toCoin "Euro"
coin3 = L.toCoin "Lira"

-- | Corresponding aTokens. We create aTokens in exchange for to the real coins
-- on our lending app
aToken1, aToken2, aToken3, aAda :: TokenName
aToken1 = Value.tokenName "aDollar"
aToken2 = Value.tokenName "aEuro"
aToken3 = Value.tokenName "aLira"
aAda    = Value.tokenName "aAda"

adaCoin = Value.AssetClass (Ada.adaSymbol, Ada.adaToken)

-- | Convert aToken to aCoin
fromToken :: TokenName -> Coin
fromToken aToken = Value.AssetClass (Forge.currencySymbol lendexId, aToken)

-- | aCoins that correspond to real coins
aCoin1, aCoin2, aCoin3 :: Coin
aCoin1 = fromToken aToken1
aCoin2 = fromToken aToken2
aCoin3 = fromToken aToken3

-- | Initial distribution of wallets for testing
initialDistribution :: M.Map Wallet Value
initialDistribution = M.fromList
  [ (wAdmin, val 2000)
  , (w1, val 1000 <> v1 100)
  , (w2, val 1000 <> v2 100)
  , (w3, val 1000 <> v3 100)
  ]
  where
    val x = Value.singleton Ada.adaSymbol Ada.adaToken x

    coinVal coin = uncurry Value.singleton (Value.unAssetClass coin)
    v1 = coinVal coin1
    v2 = coinVal coin2
    v3 = coinVal coin3

