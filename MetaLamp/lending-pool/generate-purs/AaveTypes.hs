{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module AaveTypes where

import           Language.PureScript.Bridge                 (BridgePart, Language (Haskell), SumType,
                                                             TypeInfo (TypeInfo), buildBridge, equal, genericShow,
                                                             haskType, mkSumType, order, typeModule, typeName,
                                                             writePSTypesWith, (^==))
import           Data.Proxy                                 (Proxy (Proxy))
import qualified Plutus.Contracts.Core               as Aave
import qualified Plutus.Contracts.Endpoints          as Aave
import Plutus.PAB.Simulation (AaveContracts(..))
import PlutusTx.Ratio (Ratio)
import           Language.PureScript.Bridge.TypeParameters  (A)

aaveTypes :: [SumType 'Haskell]
aaveTypes = [ (equal <*> (genericShow <*> mkSumType)) (Proxy @AaveContracts)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.Aave)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.CreateParams)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.UserContractState)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.ReserveId)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.Reserve)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.UserConfigId)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.UserConfig)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @(Ratio A))
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.DepositParams)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.WithdrawParams)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.BorrowParams)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.RepayParams) ]
