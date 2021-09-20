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

import           Control.Monad.Reader                        (MonadReader)
import           Data.Proxy                                  (Proxy (Proxy))
import           Language.PureScript.Bridge                  (BridgePart,
                                                              Language (Haskell),
                                                              PSType, SumType,
                                                              TypeInfo (TypeInfo),
                                                              buildBridge,
                                                              equal,
                                                              genericShow,
                                                              haskType,
                                                              mkSumType, order,
                                                              psTypeParameters,
                                                              typeModule,
                                                              typeName,
                                                              writePSTypesWith,
                                                              (^==))
import           Language.PureScript.Bridge.Builder          (BridgeData)
import           Language.PureScript.Bridge.TypeParameters   (A, E)
import qualified PSGenerator.Common
import           Plutus.Abstract.ContractResponse            (ContractResponse)
import qualified Plutus.Contracts.LendingPool.OffChain.Info  as Aave
import qualified Plutus.Contracts.LendingPool.OffChain.Owner as Aave
import qualified Plutus.Contracts.LendingPool.OffChain.User  as Aave
import qualified Plutus.Contracts.LendingPool.OnChain.Core   as Aave
import qualified Plutus.Contracts.Service.Oracle             as Oracle
import           Plutus.PAB.Simulation                       (AaveContracts (..))

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
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Oracle.Oracle)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @(ContractResponse E A))
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.CreateParams)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.UserContractState)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.InfoContractState)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.Reserve)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.UserConfig)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.DepositParams)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.WithdrawParams)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.BorrowParams)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.RepayParams)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.ProvideCollateralParams)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Aave.RevokeCollateralParams) ]
