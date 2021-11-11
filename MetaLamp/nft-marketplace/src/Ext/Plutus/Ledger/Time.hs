module Ext.Plutus.Ledger.Time where

import           Data.Avro.Internal.Time (utcTimeToMillis)
import qualified Data.Time.Clock         as Time
import           Plutus.V1.Ledger.Time   (DiffMilliSeconds (..), POSIXTime (..),
                                          fromMilliSeconds)

convertUtcToPOSIX :: Time.UTCTime -> POSIXTime
convertUtcToPOSIX = POSIXTime . utcTimeToMillis

-- | 'beginningOfTime' corresponds to the Shelley launch date
-- (2020-07-29T21:44:51Z) which is 1596059091000 in POSIX time
-- (number of milliseconds since 1970-01-01T00:00:00Z).
-- It is a hardcoded setting for pab simulation in Plutus code.
beginningOfTime :: POSIXTime
beginningOfTime = POSIXTime 1596059091000

newtype Seconds = Seconds Integer deriving Show

addToBeginningOfTime :: Seconds -> POSIXTime
addToBeginningOfTime (Seconds s) = beginningOfTime + fromMilliSeconds (DiffMilliSeconds $ s * 1000)
