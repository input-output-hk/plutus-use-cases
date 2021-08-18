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
import Data.Text (Text, pack)

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

import Plutus.Contract (Contract, ContractInstanceId, EmptySchema, awaitTxConfirmed, mapError, ownPubKey, submitTx, tell)
import Plutus.Contracts.Currency as Currency

import Mlabs.Plutus.PAB (call, waitForLast)
import Mlabs.System.Console.PrettyLogger (logNewLine)
import Mlabs.System.Console.Utils (logAction, logBalance, logMlabs)

cfg =
  BootstrapCfg
    { wallets = Wallet <$> [1 .. 3] -- wallets participating, wallet #1 is admin
    , govTokenName = "GOVToken" -- name of GOV token to be paid in exchange of xGOV tokens
    , govAmount = 100 -- GOV amount each wallet gets on start
    }

-- | Main function to run simulator
main :: IO ()
main =
  void $
    Handler.runSimulation (bootstrapGovernance cfg) $ do
      Simulator.logString @(Builtin GovernanceContracts) "Starting Governance PAB webserver"
      shutdown <- PWS.startServerDebug
      let simWallets = wallets cfg
          (wallet1 : wallet2 : wallet3 : _) = simWallets
      (cids, govParams) <-
        subscript
          "Initializing contracts, minting and distributing required tokens"
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
  let gov = AssetClassGov govCs $ govTokenName cfg
  cids <- forM (wallets cfg) $ \w -> Simulator.activateContract w (Governance gov)
  return (cids, gov)

-- shortcits fo endpoint calls
deposit cid amount = call cid $ Deposit amount

getBalance cid wallet = do
  call cid $ QueryBalance (pubKeyHash $ walletPubKey wallet)
  govBalance :: Integer <- waitForLast cid
  logAction $ "Balance is " ++ show govBalance

data BootstrapCfg = BootstrapCfg
  { wallets :: [Wallet]
  , govTokenName :: TokenName
  , govAmount :: Integer
  }

-- Bootstrap Contract which mints desired tokens
-- and distributes them ower wallets according to `BootstrapCfg`
bootstrapGovernance :: BootstrapCfg -> BootstrapContract
bootstrapGovernance BootstrapCfg {..} = do
  govCur <- mapError toText mintRequredTokens
  let govCs = Currency.currencySymbol govCur
      govPerWallet = Value.singleton govCs govTokenName govAmount
  distributeGov govPerWallet
  tell $ Last $ Just govCs
  where
    mintRequredTokens ::
      Contract w EmptySchema Currency.CurrencyError Currency.OneShotCurrency
    mintRequredTokens = do
      ownPK <- pubKeyHash <$> ownPubKey
      govCurrency <- Currency.mintContract ownPK [(govTokenName, govAmount * length wallets)]
      return govCurrency

    distributeGov govPerWallet = do
      ownPK <- pubKeyHash <$> ownPubKey
      forM_ wallets $ \w -> do
        let pkh = pubKeyHash $ walletPubKey w
        when (pkh /= ownPK) $ do
          tx <- submitTx $ mustPayToPubKey pkh govPerWallet
          awaitTxConfirmed $ txId tx

    toText = pack . show

printBalance :: Wallet -> Simulation (Builtin schema) ()
printBalance wallet = do
  v <- Simulator.valueAt $ walletAddress wallet
  logBalance ("WALLET " <> show wallet) v
