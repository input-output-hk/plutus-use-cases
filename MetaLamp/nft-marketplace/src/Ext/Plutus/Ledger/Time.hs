module Ext.Plutus.Ledger.Time where

import           Data.Avro.Internal.Time (utcTimeToMillis)
import qualified Data.Time.Clock         as Time
import           Plutus.V1.Ledger.Time   (POSIXTime (..))

convertUtcToPOSIX :: Time.UTCTime -> POSIXTime
convertUtcToPOSIX = POSIXTime . utcTimeToMillis
