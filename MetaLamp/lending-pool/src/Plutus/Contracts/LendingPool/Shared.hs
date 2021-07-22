{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Plutus.Contracts.LendingPool.Shared where

import           Plutus.Abstract.IncentivizedAmount               (accrue)
import           Plutus.Contracts.LendingPool.OnChain.Core.Script (Reserve (..),
                                                                   UserConfig (..))
import           Plutus.V1.Ledger.Slot                            (Slot)

updateConfigAmounts :: Reserve -> Slot -> UserConfig -> UserConfig
updateConfigAmounts Reserve{..} slot UserConfig{..} =
    UserConfig {
        ucDebt = accrue rCurrentStableBorrowRate slot ucDebt,
        ucCollateralizedInvestment = accrue rCurrentStableAccrualRate slot ucCollateralizedInvestment
    }
