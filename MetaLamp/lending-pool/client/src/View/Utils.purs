module View.Utils where

import Data.Json.JsonTuple (JsonTuple(..))
import Data.Tuple (Tuple(..))
import Plutus.V1.Ledger.Value (AssetClass(..), TokenName(..))

assetName :: AssetClass -> String
assetName (AssetClass { unAssetClass: JsonTuple (Tuple _ (TokenName { unTokenName: name })) }) = name
