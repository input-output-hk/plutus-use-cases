module View.ReserveInfo where

import Prelude
import Data.BigInteger (BigInteger)
import Halogen.HTML as HH
import Plutus.Contracts.Core (Reserve(..))
import Plutus.V1.Ledger.Value (AssetClass)
import View.Utils (assetName)

reserveInfo :: forall props act. AssetClass -> Reserve -> HH.HTML props act
reserveInfo asset (Reserve { rAmount }) = poolTab asset rAmount

poolTab :: forall props act. AssetClass -> BigInteger -> HH.HTML props act
poolTab asset amount = HH.div_ $ [ HH.h4_ [ HH.text (assetName asset <> " pool balance") ], HH.text $ show amount ]
