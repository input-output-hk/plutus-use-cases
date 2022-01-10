module Mlabs.EfficientNFT.Contract.Aux (
  getUserAddr,
  getUserUtxos,
  getAddrUtxos,
  getFirstUtxo,
) where

import PlutusTx.Prelude

import Data.Map qualified as Map
import Ledger (Address, ChainIndexTxOut, TxOutRef, pubKeyHashAddress)
import Plutus.Contract qualified as Contract

import Mlabs.EfficientNFT.Types

-- | Get the current Wallet's publick key.
getUserAddr :: GenericContract Address
getUserAddr = pubKeyHashAddress <$> Contract.ownPubKeyHash

-- | Get the current wallet's utxos.
getUserUtxos :: GenericContract (Map.Map TxOutRef ChainIndexTxOut)
getUserUtxos = getAddrUtxos =<< getUserAddr

-- | Get the ChainIndexTxOut at an address.
getAddrUtxos :: Address -> GenericContract (Map.Map TxOutRef ChainIndexTxOut)
getAddrUtxos adr = Map.map fst <$> Contract.utxosTxOutTxAt adr

-- | Get first utxo of current wallet
getFirstUtxo :: GenericContract (TxOutRef, ChainIndexTxOut)
getFirstUtxo = head . Map.toList <$> getUserUtxos
