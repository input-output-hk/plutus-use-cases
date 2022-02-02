{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# LANGUAGE TypeApplications  #-}

module Config
(
    customTraceConfig
) where

import           Control.Monad                           (forM_, when)
import qualified Data.Semigroup                          as Semigroup
import           Ledger
import           Ledger.Constraints                      hiding
                                                         (ownPaymentPubKeyHash)
import           Ledger.Value                            as Value
import           Plutus.Contract                         hiding (throwError)
import qualified Plutus.Contracts.Currency               as Currency
import           Plutus.Trace.Emulator.Types             (ContractInstanceLog (..),
                                                          ContractInstanceMsg (..))
import           Wallet.Emulator.MultiAgent              (EmulatorEvent' (..))

import qualified Data.Aeson                              as A
import           Data.Text.Prettyprint.Doc               (Pretty,
                                                          defaultLayoutOptions,
                                                          layoutPretty, pretty)
import           Data.Text.Prettyprint.Doc.Render.String (renderString)
import           Data.Void                               (Void)
import           Plutus.Trace.Emulator                   (TraceConfig (..))
import           System.IO                               (stdout)


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