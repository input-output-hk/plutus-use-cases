{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Useful utils for contracts
module Mlabs.Plutus.Contract (
  readDatum,
  readDatum',
  readChainIndexTxDatum,
  Call,
  IsEndpoint (..),
  endpointName,
  getEndpoint,
  callSimulator,
  callEndpoint',
  selectForever,
) where

import PlutusTx.Prelude
import Prelude (String)

import Control.Lens (view, (^?))
import Control.Monad (forever)
import Control.Monad.Freer (Eff)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (second)
import Data.Functor (void)
import Data.Map qualified as M
import Data.OpenUnion (Member)
import Data.Proxy (Proxy (..))
import Data.Row (KnownSymbol)
import GHC.TypeLits (Symbol, symbolVal)
import Ledger (Datum (Datum), TxOut (txOutDatumHash), TxOutTx (txOutTxOut, txOutTxTx), lookupDatum)
import Ledger.Tx (ChainIndexTxOut, ciTxOutDatum)
import Mlabs.Data.List (maybeRight)
import Playground.Contract (Contract, ToSchema)
import Plutus.ChainIndex.Tx (ChainIndexTx, citxData)
import Plutus.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (Builtin)
import Plutus.PAB.Simulator (Simulation, callEndpointOnInstance, waitNSlots)
import Plutus.Trace.Effects.RunContract (RunContract, callEndpoint)
import Plutus.Trace.Emulator.Types (ContractConstraints, ContractHandle)
import PlutusTx (FromData, fromBuiltinData)

-- | For off-chain code
readDatum :: FromData a => TxOutTx -> Maybe a
readDatum txOut = do
  h <- txOutDatumHash $ txOutTxOut txOut
  Datum e <- lookupDatum (txOutTxTx txOut) h
  PlutusTx.fromBuiltinData e

{- | For off-chain code - from querying the chain
 Using ChainIndexTxOut returned by `utxosAt`
-}
readDatum' :: FromData a => ChainIndexTxOut -> Maybe a
readDatum' txOut = do
  d <- txOut ^? ciTxOutDatum
  Datum e <- maybeRight d
  PlutusTx.fromBuiltinData e

{- | For off-chain code - from querying the chain
 Using the ChainIndexTx returned by `utxosTxOutTxAt`
-}
readChainIndexTxDatum :: FromData a => ChainIndexTx -> [Maybe a]
readChainIndexTxDatum = fmap (snd . second (\(Datum e) -> PlutusTx.fromBuiltinData e)) . M.toList . view citxData

type Call a = Contract.Endpoint (EndpointSymbol a) a

class (ToSchema a, ToJSON a, FromJSON a, KnownSymbol (EndpointSymbol a)) => IsEndpoint a where
  type EndpointSymbol a :: Symbol

callEndpoint' ::
  forall ep w s e effs.
  (IsEndpoint ep, ContractConstraints s, Contract.HasEndpoint (EndpointSymbol ep) ep s, Member RunContract effs) =>
  ContractHandle w s e ->
  ep ->
  Eff effs ()
callEndpoint' = callEndpoint @(EndpointSymbol ep)

getEndpoint ::
  forall a w s e b.
  ( Contract.HasEndpoint (EndpointSymbol a) a s
  , Contract.AsContractError e
  , IsEndpoint a
  ) =>
  (a -> Contract w s e b) ->
  Contract.Promise w s e b
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

selectForever :: [Contract.Promise w s e a] -> Contract w s e b
selectForever = forever . Contract.selectList
