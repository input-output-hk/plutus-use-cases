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

import           Control.Monad.Reader                      (MonadReader)
import           Data.Proxy                                (Proxy (Proxy))
import           Language.PureScript.Bridge                (BridgePart,
                                                            Language (Haskell),
                                                            PSType, SumType,
                                                            TypeInfo (TypeInfo),
                                                            buildBridge, equal,
                                                            genericShow,
                                                            haskType, mkSumType,
                                                            order,
                                                            psTypeParameters,
                                                            typeModule,
                                                            typeName,
                                                            writePSTypesWith,
                                                            (^==))
import           Language.PureScript.Bridge.Builder        (BridgeData)
import           Language.PureScript.Bridge.TypeParameters (A)
import qualified PSGenerator.Common
import qualified Plutus.Contracts.Core                     as Aave
import qualified Plutus.Contracts.Endpoints                as Aave
import           Plutus.PAB.Simulation                     (AaveContracts (..))
import           Plutus.V1.Ledger.Value                    (AssetClass)

ratioBridge :: BridgePart
ratioBridge = do
  typeName ^== "Ratio"
  typeModule ^== "PlutusTx.Ratio"
  psRatio

psRatio :: MonadReader BridgeData m => m PSType
psRatio = expand <$> psTypeParameters
  where
    expand [x] = TypeInfo "web-common" "Data.Json.JsonTuple" "JsonTuple" [x, x]

aaveTypes :: [SumType 'Haskell]
aaveTypes = [ (equal <*> (genericShow <*> mkSumType)) (Proxy @AaveContracts)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.Aave)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.CreateParams)
          , (order <*> (equal <*> (genericShow <*> mkSumType))) (Proxy @AssetClass)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.UserContractState)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.Reserve)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.UserConfig)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.DepositParams)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.WithdrawParams)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.BorrowParams)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.RepayParams) ]
