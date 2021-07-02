{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Useful utils for contracts
module Mlabs.Plutus.Contract(
    selects
  , readDatum
  , Call
  , IsEndpoint(..)
  , endpointName
  , getEndpoint
  , callSimulator
  , callEndpoint'
) where

import Data.Aeson (ToJSON)
import Playground.Contract (ToSchema)

import Control.Monad.Freer (Eff)
import Data.Row
import Data.OpenUnion
import Data.Proxy
import Data.Kind
import GHC.TypeLits
import Data.Functor (void)

import Prelude
import Plutus.Contract

import Ledger hiding (singleton)
import PlutusTx
import Plutus.PAB.Simulator (Simulation)
import Plutus.PAB.Simulator qualified as Simulator
import Plutus.PAB.Effects.Contract.Builtin (Builtin)
import Plutus.Trace.Emulator.Types
import Plutus.Trace.Effects.RunContract (callEndpoint, RunContract)

instance Semigroup (Contract w s e a) where
  (<>) = select

--  |Concat many endponits to one
selects :: [Contract w s e a] -> Contract w s e a
selects = foldl1 select

-- | For off-chain code
readDatum :: IsData a => TxOutTx -> Maybe a
readDatum txOut = do
  h <- txOutDatumHash $ txOutTxOut txOut
  Datum e <- lookupDatum (txOutTxTx txOut) h
  PlutusTx.fromData e

type Call a = Endpoint (EndpointSymbol a) a

class (ToSchema a, ToJSON a, KnownSymbol (EndpointSymbol a)) => IsEndpoint a where
  type EndpointSymbol a :: Symbol

callEndpoint' ::
  forall ep w s e effs.
  (IsEndpoint ep, ContractConstraints s, HasEndpoint (EndpointSymbol ep) ep s, Member RunContract effs)
    => ContractHandle w s e -> ep -> Eff effs ()
callEndpoint' hdl act = callEndpoint @(EndpointSymbol ep) hdl act

getEndpoint :: forall a w (s :: Row Type) e . (HasEndpoint (EndpointSymbol a) a s, AsContractError e, IsEndpoint a) => Contract w s e a
getEndpoint = endpoint @(EndpointSymbol a)

endpointName :: forall a . IsEndpoint a => a -> String
endpointName a = symbolVal (toProxy a)
  where
    toProxy :: a -> Proxy (EndpointSymbol a)
    toProxy _ = Proxy

callSimulator :: IsEndpoint a => ContractInstanceId -> a -> Simulation (Builtin schema) ()
callSimulator cid input = do
  void $ Simulator.callEndpointOnInstance cid (endpointName input) input
  void $ Simulator.waitNSlots 1

