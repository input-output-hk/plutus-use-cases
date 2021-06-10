module View.UsersTable where

import Prelude
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.BigInteger (fromInt)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Halogen.HTML as HH
import Plutus.Contracts.Core (UserConfig(..))
import Plutus.V1.Ledger.Value (AssetClass)
import View.Utils (assetName)

poolUsers :: forall props act. AssetClass -> NonEmptyArray (Tuple String UserConfig) -> HH.HTML props act
poolUsers asset users =
  HH.div_
    [ HH.h3_ [ HH.text $ assetName asset <> " pool:" ]
    , HH.div_ <<< toArray <<< map userInfo $ users
    ]

userInfo :: forall props act. Tuple String UserConfig -> HH.HTML props act
userInfo (Tuple userId (UserConfig { ucDebt, ucUsingAsCollateral })) =
  HH.div_
    [ HH.div_ [ HH.text $ "User " <> userId ]
    , HH.div_ [ HH.text $ "Debt: " <> (show <<< fromMaybe (fromInt 0) $ ucDebt) ]
    , HH.div_ [ HH.text $ "Using as collateral: " <> (show ucUsingAsCollateral) ]
    ]
