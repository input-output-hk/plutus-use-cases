module Ext.Plutus.Ledger.Time where

import qualified Data.Time.Clock as Time
import Data.Avro.Internal.Time (utcTimeToMillis)
import Plutus.V1.Ledger.Time (POSIXTime (..))

convertUtcToPOSIX :: Time.UTCTime -> POSIXTime
convertUtcToPOSIX = POSIXTime . utcTimeToMillis
