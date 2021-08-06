{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Useful utils for contracts
module Mlabs.Plutus.Contract (
  selects,
  readDatum,
  Call,
  IsEndpoint (..),
  endpointName,
  getEndpoint,
  callSimulator,
  callEndpoint',
) where

import PlutusTx.Prelude
import Prelude (String, foldl1)

import Control.Monad.Freer (Eff)
import Data.Aeson (ToJSON)
import Data.Functor (void)
import Data.Kind (Type)
import Data.OpenUnion (Member)
import Data.Proxy (Proxy (..))
import Data.Row (KnownSymbol, Row)
import GHC.TypeLits (Symbol, symbolVal)
import Ledger (Datum (Datum), TxOut (txOutDatumHash), TxOutTx (txOutTxOut, txOutTxTx), lookupDatum)
import Playground.Contract (Contract, ToSchema)
import Plutus.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (Builtin)
import Plutus.PAB.Simulator (Simulation, callEndpointOnInstance, waitNSlots)
import Plutus.Trace.Effects.RunContract (RunContract, callEndpoint)
import Plutus.Trace.Emulator.Types (ContractConstraints, ContractHandle)
import PlutusTx (IsData, fromBuiltinData)

instance Semigroup (Contract.Contract w s e a) where
  (<>) = Contract.select

--  |Concat many endponits to one
selects :: [Contract w s e a] -> Contract w s e a
selects = foldl1 Contract.select

-- | For off-chain code
readDatum :: IsData a => TxOutTx -> Maybe a
readDatum txOut = do
  h <- txOutDatumHash $ txOutTxOut txOut
  Datum e <- lookupDatum (txOutTxTx txOut) h
  PlutusTx.fromBuiltinData e

type Call a = Contract.Endpoint (EndpointSymbol a) a

class (ToSchema a, ToJSON a, KnownSymbol (EndpointSymbol a)) => IsEndpoint a where
  type EndpointSymbol a :: Symbol

callEndpoint' ::
  forall ep w s e effs.
  (IsEndpoint ep, ContractConstraints s, Contract.HasEndpoint (EndpointSymbol ep) ep s, Member RunContract effs) =>
  ContractHandle w s e ->
  ep ->
  Eff effs ()
callEndpoint' = callEndpoint @(EndpointSymbol ep)

getEndpoint :: forall a w (s :: Row Type) e. (Contract.HasEndpoint (EndpointSymbol a) a s, Contract.AsContractError e, IsEndpoint a) => Contract w s e a
getEndpoint = Contract.endpoint @(EndpointSymbol a)

endpointName :: forall a. IsEndpoint a => a -> String
endpointName a = symbolVal (toProxy a)
  where
    toProxy :: a -> Proxy (EndpointSymbol a)
    toProxy _ = Proxy

callSimulator :: IsEndpoint a => Contract.ContractInstanceId -> a -> Simulation (Builtin schema) ()
callSimulator cid input = do
  void $ callEndpointOnInstance cid (endpointName input) input
  void $ waitNSlots 1
