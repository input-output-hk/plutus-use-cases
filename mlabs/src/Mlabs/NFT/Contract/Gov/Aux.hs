module Mlabs.NFT.Contract.Gov.Aux (
  pointNodeTo',
  pointNodeToMaybe',
  piValue,
) where

import Data.Map qualified as Map
import Data.Maybe (fromJust)

import Plutus.ChainIndex.Tx (txOutRefMapForAddr)
import PlutusTx.Prelude hiding (mconcat, mempty, (<>))

import Ledger (
  Address,
  Value,
  txOutValue,
 )

import Mlabs.Data.LinkedList
import Mlabs.NFT.Governance.Types
import Mlabs.NFT.Types

-- `fromJust` is safe here due to on-chain constraints.
pointNodeTo' :: GovDatum -> GovDatum -> GovDatum
pointNodeTo' a b = GovDatum . fromJust $ pointNodeTo (gov'list a) (gov'list b)

pointNodeToMaybe' :: GovDatum -> Maybe GovDatum -> GovDatum
pointNodeToMaybe' a b = GovDatum . fromJust $ pointNodeToMaybe (gov'list a) (fmap gov'list b)

-- Get value attached to given `PointInfo`
piValue :: Address -> PointInfo a -> Value
piValue govAddr pi =
  Ledger.txOutValue
    . fst
    $ (txOutRefMapForAddr govAddr (pi'CITx pi) Map.! pi'TOR pi)
