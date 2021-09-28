module Utils.Time where

import Prelude
import Data.BigInteger (BigInteger, toNumber)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Formatter.DateTime as FDT
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Partial.Unsafe (unsafeCrashWith)

posixTimeToUtc :: BigInteger -> Maybe DateTime
posixTimeToUtc = map toDateTime <<< instant <<< wrap <<< toNumber

posixTimeToUtcUnsafe :: BigInteger -> DateTime
posixTimeToUtcUnsafe posix = case posixTimeToUtc posix of
  Just t -> t
  Nothing -> unsafeCrashWith "posixTimeToUtcUnsafe: NOT UTC Time"

timeFormatter :: FDT.Formatter
timeFormatter =
  fromFoldable
    [ FDT.YearTwoDigits
    , FDT.Placeholder "-"
    , FDT.MonthTwoDigits
    , FDT.Placeholder "-"
    , FDT.DayOfMonthTwoDigits
    , FDT.Placeholder " "
    , FDT.Hours24
    , FDT.Placeholder ":"
    , FDT.MinutesTwoDigits
    , FDT.Placeholder ":"
    , FDT.SecondsTwoDigits
    , FDT.Placeholder ":"
    , FDT.MillisecondsShort
    ]