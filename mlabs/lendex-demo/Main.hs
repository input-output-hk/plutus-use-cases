-- | Console demo for Lendex
module Main (
  main,
  initContract,
  activateInit,
  activateAdmin,
  activateUser,
  activateOracle,
  startParams,
  toCoin,
) where

import Prelude

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Functor (void)
import Data.Monoid (Last (..))

import Ledger.CardanoWallet (WalletNumber (..))
import Ledger.Constraints (mustPayToPubKey)
import Ledger.Crypto (PubKeyHash (..))
import Ledger.Tx (getCardanoTxId)
import Ledger.Value qualified as Value
import Playground.Contract (TokenName, Wallet (..))
import Plutus.Contract hiding (when)
import Plutus.Contracts.Currency qualified as Currency
import Plutus.PAB.Simulator qualified as Simulator
import Wallet.Emulator.Wallet (fromWalletNumber)
import Wallet.Emulator.Wallet qualified as Wallet

import Ledger (PaymentPubKeyHash (unPaymentPubKeyHash))
import Mlabs.Lending.Contract qualified as Contract
import Mlabs.Lending.Contract.Api (StartLendex (..))
import Mlabs.Lending.Contract.Simulator.Handler qualified as Handler
import Mlabs.Lending.Logic.Types hiding (User (..), Wallet (..))
import Mlabs.Plutus.PAB (call, printBalance, waitForLast)
import Mlabs.System.Console.PrettyLogger (logNewLine)
import Mlabs.System.Console.Utils (logAction, logMlabs)
import Mlabs.Utils.Wallet (walletFromNumber)
import PlutusTx.Ratio qualified as R

-- | Console demo for Lendex with simulator
main :: IO ()
main = Handler.runSimulator lendexId initContract $ do
  cur <- activateInit wAdmin
  Simulator.waitNSlots 10
  admin <- activateAdmin wAdmin
  oracle <- activateOracle wAdmin
  users <- mapM activateUser wallets

  let [user1, user2, user3] = users
      [coin1, coin2, coin3] = fmap (toCoin cur) [token1, token2, token3]

  call admin . StartLendex $ startParams cur
  next

  logMlabs
  test "Init users" (pure ())

  test
    ( unlines
        [ "Users deposit funds (100 coins in each currrency)."
        , "They receive equal amount of aTokens."
        ]
    )
    $ do
      call user1 $ Contract.Deposit 100 coin1
      call user2 $ Contract.Deposit 100 coin2
      call user3 $ Contract.Deposit 100 coin3

  test "User 1 borrows 60 Euros" $ do
    call user1 $
      Contract.AddCollateral
        { addCollateral'asset = coin1
        , addCollateral'amount = 100
        }
    call user1 $ Contract.Borrow 60 coin2 (Contract.toInterestRateFlag StableRate)

  test "User 3 withdraws 25 Liras" $ do
    call user3 $ Contract.Withdraw 25 coin3

  test
    ( unlines
        [ "Rate of Euros becomes high and User1's collateral is not enough."
        , "User2 liquidates part of the borrow"
        ]
    )
    $ do
      call oracle $ Contract.SetAssetPrice coin2 (R.fromInteger 2)
      call user2 $
        Contract.LiquidationCall
          { liquidationCall'collateral = coin1
          , liquidationCall'debtUser = toPubKeyHash w1
          , liquidationCall'debtAsset = coin2
          , liquidationCall'debtToCover = 10
          , liquidationCall'receiveAToken = True
          }

  test "User 1 repays 20 coins of the loan" $ do
    call user1 $ Contract.Repay 20 coin1 (Contract.toInterestRateFlag StableRate)

  liftIO $ putStrLn "Fin (Press enter to Exit)"
  where
    next = do
      logNewLine
      void $ Simulator.waitNSlots 10

    test msg act = do
      void act
      void $ Simulator.waitNSlots 1
      logAction msg
      mapM_ printBalance wals
      next
      where
        wals = [1, 2, 3]

initContract :: Handler.InitContract
initContract = do
  ownPK <- ownPaymentPubKeyHash
  logInfo @String "Start forge"
  cur <-
    mapError
      (Contract.toLendexError . show @Currency.CurrencyError)
      (Currency.mintContract ownPK (fmap (,amount) [token1, token2, token3]))
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
      let pkh = Wallet.mockWalletPaymentPubKeyHash w
      when (pkh /= ownPK) $ do
        tx <- submitTx $ mustPayToPubKey pkh v
        awaitTxConfirmed $ getCardanoTxId tx

-----------------------------------------------------------------------
-- activate handlers

activateInit :: Wallet -> Handler.Sim Value.CurrencySymbol
activateInit wal = do
  wid <- Simulator.activateContract wal Handler.Init
  cur <- waitForLast wid
  void $ Simulator.waitUntilFinished wid
  return cur

activateAdmin :: Wallet -> Handler.Sim ContractInstanceId
activateAdmin wal = Simulator.activateContract wal Handler.Admin

activateUser :: Wallet -> Handler.Sim ContractInstanceId
activateUser wal = Simulator.activateContract wal Handler.User

activateOracle :: Wallet -> Handler.Sim ContractInstanceId
activateOracle wal = Simulator.activateContract wal Handler.Oracle

-----------------------------------------------------------------------
-- constants

lendexId :: LendexId
lendexId = LendexId "lendex"

-- | Wallets that are used for testing.
wAdmin, w1, w2, w3 :: Wallet
wAdmin = walletFromNumber 4
w1 = walletFromNumber 1
w2 = walletFromNumber 2
w3 = walletFromNumber 3

wallets :: [Wallet]
wallets = [w1, w2, w3]

token1, token2, token3 :: TokenName
token1 = "Dollar"
token2 = "Euro"
token3 = "Lira"

{- | Corresponding aTokens. We create aTokens in exchange for to the real coins
 on our lending app
-}
aToken1, aToken2, aToken3, aAda :: TokenName
aToken1 = Value.tokenName "aDollar"
aToken2 = Value.tokenName "aEuro"
aToken3 = Value.tokenName "aLira"
aAda = Value.tokenName "aAda"

startParams :: Value.CurrencySymbol -> StartParams
startParams cur =
  StartParams
    { sp'coins =
        fmap
          ( \(coin, aCoin) ->
              CoinCfg
                { coinCfg'coin = coin
                , coinCfg'rate = R.fromInteger 1
                , coinCfg'aToken = aCoin
                , coinCfg'interestModel = defaultInterestModel
                , coinCfg'liquidationBonus = R.reduce 5 100
                }
          )
          [(adaCoin, aAda), (toCoin cur token1, aToken1), (toCoin cur token2, aToken2), (toCoin cur token3, aToken3)]
    , sp'initValue = Value.assetClassValue adaCoin 1000
    , sp'admins = [toPubKeyHash wAdmin]
    , sp'oracles = [toPubKeyHash wAdmin]
    }

toCoin :: Value.CurrencySymbol -> TokenName -> Coin
toCoin cur tn = Value.AssetClass (cur, tn)

--------------------------------------------------------------------
-- utils

toPubKeyHash :: Wallet -> PubKeyHash
toPubKeyHash = unPaymentPubKeyHash . Wallet.mockWalletPaymentPubKeyHash
