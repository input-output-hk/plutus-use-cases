module View.FundsTable where

import Prelude
import Data.BigInteger (BigInteger, fromInt)
import Data.Tuple (Tuple(..))
import Halogen.HTML as HH
import Plutus.V1.Ledger.Value (TokenName(..), Value(..))
import PlutusTx.AssocMap as Map

fundsTable :: forall props act. Value -> HH.HTML props act
fundsTable (Value ({ getValue: m })) =
  HH.div_
    $ do
        (Tuple _ amounts) <- Map.toTuples m
        (Tuple name amount) <- Map.toTuples amounts
        if amount > (fromInt 0) then
          pure $ amountTab name amount
        else
          []

amountTab :: forall props act. TokenName -> BigInteger -> HH.HTML props act
amountTab (TokenName { unTokenName: name }) amount = HH.div_ $ [ HH.text (showName name <> " " <> show amount) ]
  where
  showName "" = "ADA"

  showName n = n
