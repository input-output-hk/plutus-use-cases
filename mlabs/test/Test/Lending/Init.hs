{-# LANGUAGE NumericUnderscores #-}

-- | Init blockchain state for tests
module Test.Lending.Init (
  checkOptions,
  wAdmin,
  w1,
  w2,
  w3,
  userAct1,
  userAct2,
  userAct3,
  adaCoin,
  coin1,
  coin2,
  coin3,
  aAda,
  aToken1,
  aToken2,
  aToken3,
  aCoin1,
  aCoin2,
  aCoin3,
  initialDistribution,
  toUserId,
  toPubKeyHash,
  lendexId,
  fromToken,
) where

import Prelude

import Control.Lens ((&), (.~))
import Data.Map qualified as M
import Ledger (PaymentPubKeyHash (unPaymentPubKeyHash))
import Plutus.Contract.Test (CheckOptions, Wallet (..), defaultCheckOptions, emulatorConfig, mockWalletPaymentPubKeyHash)
import Plutus.Trace.Emulator (EmulatorTrace, initialChainState)
import Plutus.V1.Ledger.Ada qualified as Ada
import Plutus.V1.Ledger.Crypto (PubKeyHash (..))
import Plutus.V1.Ledger.Value (TokenName, Value)
import Plutus.V1.Ledger.Value qualified as Value

import Mlabs.Lending.Contract.Emulator.Client qualified as L
import Mlabs.Lending.Contract.Forge (currencySymbol)
import Mlabs.Lending.Logic.App qualified as L
import Mlabs.Lending.Logic.Types (Coin, LendexId (..), UserAct (..), UserId (..))
import Mlabs.Utils.Wallet (walletFromNumber)

checkOptions :: CheckOptions
checkOptions = defaultCheckOptions & emulatorConfig . initialChainState .~ Left initialDistribution

-- | Wallets that are used for testing.
wAdmin, w1, w2, w3 :: Wallet
w1 = walletFromNumber 1
w2 = walletFromNumber 2
w3 = walletFromNumber 3
wAdmin = walletFromNumber 4

toUserId :: Wallet -> UserId
toUserId = UserId . mockWalletPaymentPubKeyHash

toPubKeyHash :: Wallet -> PubKeyHash
toPubKeyHash = unPaymentPubKeyHash . mockWalletPaymentPubKeyHash

-- | Identifier for our lendex platform
lendexId :: LendexId
lendexId = LendexId "MLabs lending platform"

-- | Showrtcuts for user actions
userAct1, userAct2, userAct3 :: UserAct -> EmulatorTrace ()
userAct1 = L.callUserAct lendexId w1
userAct2 = L.callUserAct lendexId w2
userAct3 = L.callUserAct lendexId w3

-- | Coins which are used for testing
adaCoin, coin1, coin2, coin3 :: Coin
coin1 = L.toCoin "Dollar"
coin2 = L.toCoin "Euro"
coin3 = L.toCoin "Lira"

{- | Corresponding aTokens. We create aTokens in exchange for to the real coins
 on our lending app
-}
aToken1, aToken2, aToken3, aAda :: TokenName
aToken1 = Value.tokenName "aDollar"
aToken2 = Value.tokenName "aEuro"
aToken3 = Value.tokenName "aLira"
aAda = Value.tokenName "aAda"

adaCoin = Value.AssetClass (Ada.adaSymbol, Ada.adaToken)

-- | Convert aToken to aCoin
fromToken :: TokenName -> Coin
fromToken aToken = Value.AssetClass (currencySymbol lendexId, aToken)

-- | aCoins that correspond to real coins
aCoin1, aCoin2, aCoin3 :: Coin
aCoin1 = fromToken aToken1
aCoin2 = fromToken aToken2
aCoin3 = fromToken aToken3

-- | Initial distribution of wallets for testing
initialDistribution :: M.Map Wallet Value
initialDistribution =
  M.fromList
    [ (wAdmin, val 2000_000_000)
    , (w1, val 1000_000_000 <> v1 100)
    , (w2, val 1000_000_000 <> v2 100)
    , (w3, val 1000_000_000 <> v3 100)
    ]
  where
    val x = Value.singleton Ada.adaSymbol Ada.adaToken x

    coinVal coin = uncurry Value.singleton (Value.unAssetClass coin)
    v1 = coinVal coin1
    v2 = coinVal coin2
    v3 = coinVal coin3
