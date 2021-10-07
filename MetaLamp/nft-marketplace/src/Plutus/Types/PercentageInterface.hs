module Plutus.Abstract.PercentageInterface (calculatePercentageRounded) where
import qualified Data.Aeson                 as J
import           GHC.Generics               (Generic)
import qualified Plutus.Abstract.Percentage as Percentage
import           PlutusTx.Prelude           ((*))
import           PlutusTx.Ratio             as Ratio
import           Prelude                    hiding ((*))

{-# INLINABLE calculatePercentageRounded #-}
calculatePercentageRounded :: Percentage.Percentage -> Integer -> Integer
calculatePercentageRounded (Percentage.Percentage (numerator, denominator)) percentageBy =
      Ratio.round $ (percentageBy % 100) * (numerator % denominator)
