module Mlabs.Plutus.PAB (
  call,
  waitForLast,
  printBalance,
) where

import Prelude

import Data.Aeson (FromJSON, Result (..), fromJSON)
import Data.Functor (void)
import Data.Monoid (Last (..))
import Mlabs.Utils.Wallet (walletFromNumber)
import Plutus.Contract (ContractInstanceId)
import Plutus.PAB.Effects.Contract.Builtin (Builtin)
import Plutus.PAB.Simulator (Simulation, callEndpointOnInstance, valueAt, waitForState, waitNSlots)
import Wallet.Emulator.Wallet qualified as Wallet

import Mlabs.Plutus.Contract (IsEndpoint, endpointName)
import Mlabs.System.Console.Utils (logBalance)

call :: IsEndpoint a => ContractInstanceId -> a -> Simulation (Builtin schema) ()
call cid input = do
  void $ callEndpointOnInstance cid (endpointName input) input
  void $ waitNSlots 2

{- | Waits for the given value to be written to the state of the service.
 We use it to share data between endpoints. One endpoint can write parameter to state with tell
 and in another endpoint we wait for the state-change.
-}
waitForLast :: FromJSON a => ContractInstanceId -> Simulation t a
waitForLast cid =
  flip waitForState cid $ \json -> case fromJSON json of
    Success (Last (Just x)) -> Just x
    _ -> Nothing

printBalance :: Integer -> Simulation (Builtin schema) ()
printBalance n =
  logBalance ("WALLET " <> show n) =<< (valueAt . Wallet.walletAddress $ walletFromNumber n)
