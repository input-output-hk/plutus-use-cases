{-# LANGUAGE OverloadedLists #-}

-- | Server for governance application
module Mlabs.Governance.Contract.Server (
    GovernanceContract
  , governanceEndpoints
  ) where

import PlutusTx.Prelude

import Data.Text (Text)
import Text.Printf (printf)
import Control.Monad (forever, guard, void)
import Data.Semigroup (Last(..), sconcat)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Crypto (pubKeyHash)
import Ledger.Contexts (scriptCurrencySymbol)
import Plutus.V1.Ledger.Tx (txId)
import Plutus.V1.Ledger.Value (CurrencySymbol)
import Ledger.Constraints qualified as Constraints

import Mlabs.Governance.Contract.Api qualified as Api
import Mlabs.Governance.Contract.Validation qualified as Validation
import Mlabs.Plutus.Contract (getEndpoint, readDatum, selects)

-- do we want another error type? 
type GovernanceContract a = Contract.Contract (Maybe (Last Integer)) Api.GovernanceSchema Text a

governanceEndpoints :: CurrencySymbol -> GovernanceContract ()
governanceEndpoints csym = forever $ selects
  [ getEndpoint @Api.Deposit >>= deposit csym
  , getEndpoint @Api.Withdraw >>= withdraw csym 
  , getEndpoint @Api.ProvideRewards >>= provideRewards csym
  , getEndpoint @Api.QueryBalance >>= queryBalance csym
  ]

--- actions

deposit :: CurrencySymbol -> Api.Deposit -> GovernanceContract ()
deposit csym (Api.Deposit amnt) = do
  let mintingPolicy = Validation.xGovMintingPolicy csym
      tx = sconcat [
          Constraints.mustForgeValue        $ Validation.xgovValueOf (scriptCurrencySymbol mintingPolicy) amnt
        , Constraints.mustPayToTheScript () $ Validation.govValueOf csym amnt -- here () is the datum type, for now
        ]
      lookups = sconcat [
              Constraints.monetaryPolicy        (Validation.xGovMintingPolicy csym)
            , Constraints.otherScript           (Validation.scrValidator csym)
            , Constraints.scriptInstanceLookups (Validation.scrInstance csym)
            ]
  ledgerTx <- Contract.submitTxConstraintsWith @Validation.Governance lookups tx
  void $ Contract.awaitTxConfirmed $ txId ledgerTx
  Contract.logInfo @String $ printf "deposited %s GOV tokens" (show amnt)

withdraw :: CurrencySymbol -> Api.Withdraw -> GovernanceContract ()
withdraw csym (Api.Withdraw amnt) = do
  pkh   <- pubKeyHash <$> Contract.ownPubKey
  let tx = sconcat [
          Constraints.mustPayToTheScript () $ Validation.xgovValueOf csym amnt -- here () is the datum type, for now
        , Constraints.mustPayToPubKey pkh   $ Validation.govValueOf csym amnt
        ]
      lookups = sconcat [
              Constraints.otherScript    (Validation.scrValidator csym)
            , Constraints.scriptInstanceLookups (Validation.scrInstance csym)
            ]
  ledgerTx <- Contract.submitTxConstraintsWith @Validation.Governance lookups tx
  void $ Contract.awaitTxConfirmed $ txId ledgerTx
  Contract.logInfo @String $ printf "withdrew %s GOV tokens" (show amnt)

provideRewards :: CurrencySymbol -> Api.ProvideRewards -> GovernanceContract ()
provideRewards = undefined

queryBalance :: CurrencySymbol -> Api.QueryBalance -> GovernanceContract ()
queryBalance = undefined
