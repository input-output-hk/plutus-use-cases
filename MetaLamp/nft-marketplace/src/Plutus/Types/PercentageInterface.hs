module Plutus.Types.PercentageInterface (calculatePercentageRounded) where
import qualified Data.Aeson     as J
import           GHC.Generics   (Generic)
import PlutusTx.Prelude ((*))
import           Prelude hiding ((*))
import qualified Plutus.Types.Percentage as Percentage
import PlutusTx.Ratio as Ratio

{-# INLINABLE calculatePercentageRounded #-}
calculatePercentageRounded :: Percentage.Percentage -> Integer -> Integer
calculatePercentageRounded (Percentage.Percentage (numerator, denominator)) percentageBy = 
      Ratio.round $ (percentageBy % 100) * (numerator % denominator)