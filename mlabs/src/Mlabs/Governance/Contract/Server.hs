-- | Server for governance application
module Mlabs.Governance.Contract.Server (
    GovernanceContract
  , governanceEndpoints
  ) where

import PlutusTx.Prelude

import Data.Text (Text)
import Text.Printf (printf)
import Control.Monad (forever, guard, void)
import Data.Semigroup (Last(..))
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Crypto (pubKeyHash)
import Plutus.V1.Ledger.Tx (txId)
import Ledger.Constraints qualified as Constraints

import Mlabs.Governance.Contract.Api qualified as Api
import Mlabs.Governance.Contract.Validation qualified as Validation
import Mlabs.Plutus.Contract (getEndpoint, readDatum, selects)

-- do we want another error type? 
type GovernanceContract a = Contract.Contract (Maybe (Last Integer)) Api.GovernanceSchema Text a

governanceEndpoints :: GovernanceContract ()
governanceEndpoints = forever $ selects
  [ getEndpoint @Api.Deposit >>= deposit
  , getEndpoint @Api.Withdraw >>= withdraw
  , getEndpoint @Api.ProvideRewards >>= provideRewards
  , getEndpoint @Api.QueryBalance >>= queryBalance
  ]

--- actions

deposit :: Api.Deposit -> GovernanceContract ()
deposit (Api.Deposit amnt) = do
  let tx = Constraints.mustPayToTheScript () $ Validation.govValueOf amnt -- here () is the datum type, for now
  ledgerTx <- Contract.submitTxConstraints Validation.inst tx
  void $ Contract.awaitTxConfirmed $ txId ledgerTx
  Contract.logInfo @String $ printf "deposited %s GOV tokens" (show amnt)

withdraw :: Api.Withdraw -> GovernanceContract ()
withdraw = undefined

provideRewards :: Api.ProvideRewards -> GovernanceContract ()
provideRewards = undefined

queryBalance :: Api.QueryBalance -> GovernanceContract ()
queryBalance = undefined
