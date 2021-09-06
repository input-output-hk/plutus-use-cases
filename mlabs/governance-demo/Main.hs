-- | Simulator demo for Governance
module Main (
  main,
) where

import PlutusTx.Prelude
import Prelude (IO, getLine, show, undefined)

import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, Result (Success), encode, fromJSON)
import Data.Functor (void)
import Data.Monoid (Last (..))

import Mlabs.Governance.Contract.Api (Deposit (..), QueryBalance (..), Withdraw (..))
import Mlabs.Governance.Contract.Simulator.Handler (BootstrapContract, GovernanceContracts (..))
import Mlabs.Governance.Contract.Simulator.Handler qualified as Handler
import Mlabs.Governance.Contract.Validation (AssetClassGov (..))

import Ledger (CurrencySymbol, PubKeyHash, TokenName, pubKeyHash, txId)
import Ledger.Constraints (mustPayToPubKey)
import Plutus.V1.Ledger.Value qualified as Value

import Plutus.PAB.Effects.Contract.Builtin (Builtin)
import Plutus.PAB.Simulator (Simulation)
import Plutus.PAB.Simulator qualified as Simulator
import Plutus.PAB.Webserver.Server qualified as PWS
import Wallet.Emulator.Types (Wallet (..), walletPubKey)
import Wallet.Emulator.Wallet (walletAddress)

import Mlabs.Plutus.PAB (call, waitForLast)
import Mlabs.System.Console.PrettyLogger (logNewLine)
import Mlabs.System.Console.Utils (logAction, logBalance, logMlabs)

-- | Main function to run simulator
main :: IO ()
main = void $
  Simulator.runSimulationWith Handler.handlers $ do
    Simulator.logString @(Builtin GovernanceContracts) "Starting Governance PAB webserver"
    shutdown <- PWS.startServerDebug
    let simWallets = Handler.wallets
        (wallet1 : wallet2 : wallet3 : _) = simWallets
    (cids, gov) <-
      subscript
        "Initializing contracts\nWallet 1 mints and distributes initial GOV tokens"
        simWallets
        (itializeContracts wallet1)
    let [wCid1, wCid2, wCid3] = cids

    subscript_
      "Wallet 2 deposits 55 GOV (xGOV tokens being minted as result) "
      simWallets
      $ deposit wCid2 55

    subscript_
      "Wallet 2 queries amount of GOV deposited"
      simWallets
      $ getBalance wCid2 wallet2

    subscript_
      "Wallet 2 deposits 10 more GOV"
      simWallets
      $ deposit wCid2 10

    subscript_
      "Wallet 2 queries amount of GOV deposited"
      simWallets
      $ getBalance wCid2 wallet2

    subscript_
      "Wallet 2 withdraws 60 GOV"
      simWallets
      $ withdraw wCid2 wallet2 60

    subscript_
      "Wallet 2 queries amount of GOV deposited"
      simWallets
      $ getBalance wCid2 wallet2

    subscript_
      "Finally, Wallet 3 queries amount of GOV deposited"
      simWallets
      $ getBalance wCid3 wallet3

    Simulator.logString @(Builtin GovernanceContracts) "Scripted part is over\nPress Enter to stop and exit"
    void $ liftIO getLine
    shutdown
  where
    subscript_ msg wallets simulation = void $ subscript msg wallets simulation
    subscript msg wallets simulation = do
      logAction msg
      next
      res <- simulation
      Simulator.waitNSlots 1
      mapM_ printBalance wallets
      next
      return res

    next = do
      logNewLine
      void $ Simulator.waitNSlots 5

-- shortcut for Governance initialization
itializeContracts admin = do
  cidInit <- Simulator.activateContract admin Bootstrap
  govCs <- waitForLast cidInit
  void $ Simulator.waitUntilFinished cidInit
  let gov = AssetClassGov govCs Handler.govTokenName
  cids <- forM Handler.wallets $ \w -> Simulator.activateContract w (Governance gov)
  return (cids, gov)

-- shortcits for endpoint calls
deposit cid amount = call cid $ Deposit amount

withdraw cid wallet amount = call cid $ Withdraw [(walletPKH wallet, amount)]

getBalance cid wallet = do
  call cid $ QueryBalance $ walletPKH wallet
  govBalance :: Integer <- waitForLast cid
  logAction $ "Balance is " ++ show govBalance

printBalance :: Wallet -> Simulation (Builtin schema) ()
printBalance wallet = do
  v <- Simulator.valueAt $ walletAddress wallet
  logBalance ("WALLET " <> show wallet) v

walletPKH = pubKeyHash . walletPubKey

-- cfg =
--   BootstrapCfg
--     { wallets = Wallet <$> [1 .. 3] -- wallets participating, wallet #1 is admin
--     , govTokenName = "GOVToken" -- name of GOV token to be paid in exchange of xGOV tokens
--     , govAmount = 100 -- GOV amount each wallet gets on start
--     }
