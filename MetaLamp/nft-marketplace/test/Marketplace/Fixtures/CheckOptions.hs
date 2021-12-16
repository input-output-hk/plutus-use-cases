{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TupleSections      #-}

module Marketplace.Fixtures.CheckOptions where

import           Control.Lens                                 ((&), (.~))
import           Data.Default                                 (Default (def))
import qualified Data.Map                                     as Map
import           Ext.Plutus.Ledger.Time                       (beginningOfTime)
import           Ledger                                       (Value)
import qualified Ledger.Ada                                   as Ada
import           Ledger.TimeSlot                              (SlotConfig (..))
import qualified Ledger.Value                                 as V
import qualified Marketplace.Fixtures.Wallet                  as Fixtures
import           Plutus.Contract.Test
import           Plutus.Contract.Trace                        (defaultDist)
import qualified Plutus.Trace                                 as Trace
import           Plutus.V1.Ledger.Time                        (POSIXTime (..))

options :: CheckOptions
options = defaultCheckOptions & emulatorConfig .~ emulatorCfg
  where
    emulatorCfg :: Trace.EmulatorConfig
    emulatorCfg = Trace.EmulatorConfig (Left defaultDist) slotConfiguration def

slotConfiguration :: SlotConfig
slotConfiguration = SlotConfig
        { scSlotLength   = 1000
        , scSlotZeroTime = beginningOfTime
        }
