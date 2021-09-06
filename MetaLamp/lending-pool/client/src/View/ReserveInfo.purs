module View.ReserveInfo where

import Prelude
import Data.BigInteger (BigInteger, fromInt, toNumber)
import Halogen.HTML as HH
import Plutus.Contracts.LendingPool.OnChain.Core.Script (Reserve(..))
import Plutus.V1.Ledger.Value (AssetClass)
import View.Utils (assetName)
import Data.Json.JsonTuple (JsonTuple(..))
import Data.Tuple (Tuple(..))

reserveInfo :: forall props act. AssetClass -> Reserve -> HH.HTML props act
reserveInfo asset (Reserve { rAmount, rCurrentStableBorrowRate, rLiquidityRate }) = 
    HH.div_ [poolBalance asset rAmount, poolRates rCurrentStableBorrowRate rLiquidityRate]

poolBalance :: forall props act. AssetClass -> BigInteger -> HH.HTML props act
poolBalance asset amount = HH.div_ $ [ HH.h4_ [ HH.text (assetName asset <> " pool balance") ], HH.text $ show amount ]

poolRates :: forall props act. JsonTuple BigInteger BigInteger -> JsonTuple BigInteger BigInteger -> HH.HTML props act
poolRates borrowRate incomeRate = HH.div_ $ [ HH.text $ "Borrow rate: " <> showPercent borrowRate <> " Income rate: " <> showPercent incomeRate ]

showPercent :: JsonTuple BigInteger BigInteger -> String
showPercent = (_ <> "%") <<< show <<< ratioToPercent

ratioToPercent :: JsonTuple BigInteger BigInteger -> Number
ratioToPercent (JsonTuple (Tuple a b)) = toNumber (a * (fromInt 100)) / (toNumber b)
