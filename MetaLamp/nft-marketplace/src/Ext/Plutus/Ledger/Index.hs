module Ext.Plutus.Ledger.Index where

import Ledger.Index (minAdaTxOut)
import           Plutus.V1.Ledger.Value (Value)
import Plutus.V1.Ledger.Ada (Ada, lovelaceValueOf, fromValue, toValue)

-- TODO: That should be configurable in future:
-- Read minUTxOValue from `testnet-shelley-genesis.json` cardano-node config
minAdaTxOutValue :: Value
minAdaTxOutValue = toValue minAdaTxOut
