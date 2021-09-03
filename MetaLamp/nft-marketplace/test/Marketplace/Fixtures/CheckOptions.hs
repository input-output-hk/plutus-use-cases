{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TupleSections      #-}

module Marketplace.Fixtures.CheckOptions where

import           Control.Lens                                 ((&), (.~))
import qualified Data.Map                                     as Map
import           Ledger                                       (Value)
import qualified Ledger.Ada                                   as Ada
import qualified Ledger.Value                                 as V
import qualified Marketplace.Fixtures.Wallet                  as Fixtures
import           Plutus.Contract.Test
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Marketplace
import qualified Plutus.Trace                                 as Trace
import           Data.Default                        (Default (def))

options :: CheckOptions
options = defaultCheckOptions & emulatorConfig .~ emulatorCfg
                              & maxSlot .~ 15_000
  where
    emulatorCfg :: Trace.EmulatorConfig
    emulatorCfg = def
    