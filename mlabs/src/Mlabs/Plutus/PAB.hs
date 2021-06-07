module Mlabs.Plutus.PAB(
    call
  , waitForLast
  , printBalance
) where

import Prelude
import Data.Aeson (FromJSON, Result(..), fromJSON)
import Data.Functor (void)
import Data.Monoid (Last(..))

import Plutus.Contract
import Plutus.PAB.Simulator (Simulation)
import Plutus.PAB.Simulator qualified as Simulator
import Wallet.Emulator.Wallet (Wallet(..))
import Wallet.Emulator.Wallet qualified as Wallet

import Mlabs.Plutus.Contract
import Mlabs.System.Console.Utils
import Plutus.PAB.Effects.Contract.Builtin (Builtin)

call :: IsEndpoint a => ContractInstanceId -> a -> Simulation (Builtin schema) ()
call cid input = do
  void $ Simulator.callEndpointOnInstance cid (endpointName input) input
  void $ Simulator.waitNSlots 2

-- | Waits for the given value to be written to the state of the service.
-- We use it to share data between endpoints. One endpoint can write parameter to state with tell
-- and in another endpoint we wait for the state-change.
waitForLast :: FromJSON a => ContractInstanceId -> Simulator.Simulation t a
waitForLast cid =
  flip Simulator.waitForState cid $ \json -> case fromJSON json of
    Success (Last (Just x)) -> Just x
    _                       -> Nothing

printBalance :: Integer -> Simulation (Builtin schema) ()
printBalance n =
  logBalance ("WALLET " <> show n) =<< Simulator.valueAt (Wallet.walletAddress (Wallet n))

