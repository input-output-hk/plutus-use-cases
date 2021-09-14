module Utils.Time where

import Prelude
import Data.BigInteger (BigInteger, toNumber)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (wrap)
import Partial.Unsafe (unsafeCrashWith)

posixTimeToUtc :: BigInteger -> Maybe DateTime
posixTimeToUtc = map toDateTime <<< instant <<< wrap <<< toNumber

posixTimeToUtcUnsafe :: BigInteger -> DateTime
posixTimeToUtcUnsafe = fromMaybe (unsafeCrashWith "posixTimeToUtcUnsafe: NOT UTC Time") <<< posixTimeToUtc
