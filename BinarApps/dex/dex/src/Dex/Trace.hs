{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}

module Dex.Trace
  ( customTraceConfig
  , setupTokens
  , tokenNames
  , wallets
  ) where

import           Control.Monad                           (forM_, when)
import qualified Data.Semigroup                          as Semigroup
import           Ledger
import           Ledger.Constraints                      hiding (ownPubKeyHash)
import           Ledger.Value                            as Value
import           Plutus.Contract                         hiding (throwError)
import qualified Plutus.Contracts.Currency               as Currency
import           Plutus.Trace.Emulator.Types             (ContractInstanceLog (..),
                                                          ContractInstanceMsg (..))
import           Wallet.Emulator.MultiAgent              (EmulatorEvent' (..))
import           Wallet.Emulator.Types                   (Wallet, knownWallet,
                                                          walletPubKeyHash)

import qualified Data.Aeson                              as A
import           Data.Text.Prettyprint.Doc               (Pretty,
                                                          defaultLayoutOptions,
                                                          layoutPretty, pretty)
import           Data.Text.Prettyprint.Doc.Render.String (renderString)
import           Plutus.Trace.Emulator                   (TraceConfig (..))
import           System.IO                               (stdout)


-- | Create some sample tokens and distribute them to
--   the emulated wallets
setupTokens :: Contract (Maybe (Semigroup.Last Currency.OneShotCurrency)) Currency.CurrencySchema Currency.CurrencyError ()
setupTokens = do
    ownPK <- ownPubKeyHash
    cur   <- Currency.mintContract ownPK [(tn, fromIntegral (length wallets) * amount) | tn <- tokenNames]
    let cs = Currency.currencySymbol cur
        v  = mconcat [Value.singleton cs tn amount | tn <- tokenNames]
    forM_ wallets $ \w -> do
        let pkh = walletPubKeyHash w
        when (pkh /= ownPK) $ do
            tx <- submitTx $ mustPayToPubKey pkh v
            awaitTxConfirmed $ txId tx
    tell $ Just $ Semigroup.Last cur
  where
    amount = 1000000

wallets :: [Wallet]
wallets = knownWallet <$> [1 .. 4]

tokenNames :: [TokenName]
tokenNames = ["A", "B", "C", "D"]

defaultShowEvent :: EmulatorEvent' -> Maybe String
defaultShowEvent = \case
  UserThreadEvent msg                                                  -> Just $ "*** USER LOG: " <> render msg
  InstanceEvent (ContractInstanceLog (ContractLog (A.String msg)) _ _) -> Just $ "*** CONTRACT LOG: " <> show msg
  InstanceEvent (ContractInstanceLog (StoppedWithError err)       _ _) -> Just $ "*** CONTRACT STOPPED WITH ERROR: " <> show err
  InstanceEvent (ContractInstanceLog NoRequestsHandled            _ _) -> Nothing
  InstanceEvent (ContractInstanceLog (HandledRequest _)           _ _) -> Nothing
  InstanceEvent (ContractInstanceLog (CurrentRequests _)          _ _) -> Nothing
  SchedulerEvent _                                                     -> Nothing
  ChainIndexEvent _ _                                                  -> Nothing
  ev                                                                   -> Just . render $ ev
  where
      render :: Pretty a => a -> String
      render = renderString . layoutPretty defaultLayoutOptions . pretty


customTraceConfig :: TraceConfig
customTraceConfig =
  TraceConfig
    { showEvent = defaultShowEvent,
      outputHandle = stdout
    }
