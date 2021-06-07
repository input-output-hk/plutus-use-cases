-- | Console demo for Lendex
module Main where

import Prelude

import Control.Monad (when)

import Control.Monad.IO.Class
import Data.Functor
import Data.Monoid (Last(..))

import Plutus.V1.Ledger.Crypto (PubKeyHash(..))
import Plutus.V1.Ledger.Contexts (pubKeyHash)
import Playground.Contract
import Plutus.V1.Ledger.Value (CurrencySymbol)
import qualified Plutus.V1.Ledger.Value as Value
import Plutus.PAB.Simulator qualified as Simulator
import Wallet.Emulator.Wallet qualified as Wallet

import Ledger.Constraints
import Plutus.V1.Ledger.Tx
import Plutus.Contract hiding (when)

import Mlabs.Plutus.PAB
import qualified Mlabs.Data.Ray as R
import Mlabs.System.Console.PrettyLogger

import Mlabs.Lending.Logic.Types hiding (Wallet(..), User(..))
import Mlabs.Lending.Contract

import qualified Plutus.Contracts.Currency as Currency

import Mlabs.Lending.Contract.Simulator.Handler
import Mlabs.System.Console.Utils

-- | Console demo for Lendex with simulator
main :: IO ()
main = runSimulator lendexId initContract $ do
  cur    <- activateInit wAdmin
  Simulator.waitNSlots 10
  admin  <- activateAdmin wAdmin
  oracle <- activateOracle wAdmin
  users  <- mapM activateUser wallets

  let [user1, user2, user3] = users
      [coin1, coin2, coin3] = fmap (toCoin cur) [token1, token2, token3]

  call admin $ startParams cur
  next

  logMlabs
  test "Init users" (pure ())

  test (unlines [ "Users deposit funds (100 coins in each currrency)."
                , "They receive equal amount of aTokens."]
       ) $ do
    call user1 $ Deposit 100 coin1
    call user2 $ Deposit 100 coin2
    call user3 $ Deposit 100 coin3

  test "User 1 borrows 60 Euros" $ do
    call user1 $ SetUserReserveAsCollateral
                  { setCollateral'asset           = coin1
                  , setCollateral'useAsCollateral = True
                  , setCollateral'portion         = 1 R.% 1
                  }
    call user1 $ Borrow 60 coin2 (toInterestRateFlag StableRate)

  test "User 3 withdraws 25 Liras" $ do
    call user3 $ Withdraw 25 coin3

  test (unlines [ "Rate of Euros becomes high and User1's collateral is not enough."
                , "User2 liquidates part of the borrow"]
       ) $ do
    call oracle $ SetAssetPrice coin2 (R.fromInteger 2)
    call user2 $ LiquidationCall
                  { liquidationCall'collateral     = coin1
                  , liquidationCall'debtUser       = (toPubKeyHash w1)
                  , liquidationCall'debtAsset      = coin2
                  , liquidationCall'debtToCover    = 10
                  , liquidationCall'receiveAToken  = True
                  }

  test "User 1 repays 20 coins of the loan" $ do
    call user1 $ Repay 20 coin1 (toInterestRateFlag StableRate)

  liftIO $ putStrLn "Fin (Press enter to Exit)"
  where
    next = do
      logNewLine
      void $ Simulator.waitNSlots 10

    test msg act = do
      void $ act
      void $ Simulator.waitNSlots 1
      logAction msg
      mapM_ printBalance wals
      next
      where
        wals = [1,2,3]

initContract :: InitContract
initContract = do
  ownPK <- pubKeyHash <$> ownPubKey
  logInfo @String "Start forge"
  cur   <-
      mapError (toLendexError . show @Currency.CurrencyError)
      (Currency.forgeContract ownPK (fmap (, amount) [token1, token2, token3]))
  let cs = Currency.currencySymbol cur
  tell $ Last (Just cs)
  logInfo @String "Forged coins"
  giveTo ownPK w1 (toVal cs token1)
  giveTo ownPK w2 (toVal cs token2)
  giveTo ownPK w3 (toVal cs token3)
  logInfo @String "Gave money to users"
  where
    amount :: Integer
    amount = 1000

    toVal cs tn = Value.singleton cs tn amount

    giveTo ownPK w v = do
      let pkh = pubKeyHash $ Wallet.walletPubKey w
      when (pkh /= ownPK) $ do
          tx <- submitTx $ mustPayToPubKey pkh v
          awaitTxConfirmed $ txId tx

-----------------------------------------------------------------------
-- activate handlers

activateInit :: Wallet -> Sim CurrencySymbol
activateInit wal = do
  wid <- Simulator.activateContract wal Init
  cur <- waitForLast wid
  void $ Simulator.waitUntilFinished wid
  pure cur

activateAdmin :: Wallet -> Sim ContractInstanceId
activateAdmin wal = Simulator.activateContract wal Admin

activateUser :: Wallet -> Sim ContractInstanceId
activateUser wal = Simulator.activateContract wal User

activateOracle :: Wallet -> Sim ContractInstanceId
activateOracle wal = Simulator.activateContract wal Oracle

-----------------------------------------------------------------------
-- constants

lendexId :: LendexId
lendexId = LendexId "lendex"

-- | Wallets that are used for testing.
wAdmin, w1, w2, w3 :: Wallet
wAdmin = Wallet 4
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3

wallets :: [Wallet]
wallets = [w1, w2, w3]

token1, token2, token3 :: TokenName
token1 = "Dollar"
token2 = "Euro"
token3 = "Lira"

-- | Corresponding aTokens. We create aTokens in exchange for to the real coins
-- on our lending app
aToken1, aToken2, aToken3, aAda :: TokenName
aToken1 = Value.tokenName "aDollar"
aToken2 = Value.tokenName "aEuro"
aToken3 = Value.tokenName "aLira"
aAda    = Value.tokenName "aAda"

startParams :: CurrencySymbol -> StartParams
startParams cur = StartParams
  { sp'coins = fmap (\(coin, aCoin) -> CoinCfg
                                        { coinCfg'coin = coin
                                        , coinCfg'rate = R.fromInteger 1
                                        , coinCfg'aToken = aCoin
                                        , coinCfg'interestModel = defaultInterestModel
                                        , coinCfg'liquidationBonus = 5 R.% 100
                                        })
        [(adaCoin, aAda), (toCoin cur token1, aToken1), (toCoin cur token2, aToken2), (toCoin cur token3, aToken3)]
  , sp'initValue = Value.assetClassValue adaCoin 1000
  , sp'admins    = [toPubKeyHash wAdmin]
  , sp'oracles   = [toPubKeyHash wAdmin]
  }
  where

toCoin :: CurrencySymbol -> TokenName -> Coin
toCoin cur tn = Value.AssetClass (cur, tn)

--------------------------------------------------------------------
-- utils

toPubKeyHash :: Wallet -> PubKeyHash
toPubKeyHash = pubKeyHash . Wallet.walletPubKey

