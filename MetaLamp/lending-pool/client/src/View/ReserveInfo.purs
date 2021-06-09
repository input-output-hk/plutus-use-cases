module View.ReserveInfo where

import Prelude
import Data.BigInteger (BigInteger)
import Data.Json.JsonTuple (JsonTuple(..))
import Data.Tuple (Tuple(..))
import Halogen.HTML as HH
import Plutus.Contracts.Core (Reserve(..))
import Plutus.V1.Ledger.Value (AssetClass(..), TokenName(..))

reserveInfo :: forall props act. AssetClass -> Reserve -> HH.HTML props act
reserveInfo (AssetClass { unAssetClass: JsonTuple (Tuple _ name) }) (Reserve { rAmount }) = poolTab name rAmount

poolTab :: forall props act. TokenName -> BigInteger -> HH.HTML props act
poolTab (TokenName { unTokenName: name }) amount = HH.div_ $ [ HH.h4_ [ HH.text (name <> " pool balance") ], HH.text $ show amount ]
