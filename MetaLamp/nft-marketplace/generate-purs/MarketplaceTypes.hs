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

module MarketplaceTypes where

import           Control.Monad.Reader                         (MonadReader)
import           Data.Proxy                                   (Proxy (Proxy))
import qualified Ext.Plutus.Contracts.Auction                 as Auction
import           Language.PureScript.Bridge                   (BridgePart,
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
import           Language.PureScript.Bridge.Builder           (BridgeData)
import           Language.PureScript.Bridge.TypeParameters    (A, E)
import           Plutus.Abstract.ContractResponse             (ContractResponse)
import           Plutus.Contract.StateMachine.ThreadToken     (ThreadToken)
import qualified Plutus.Contracts.NftMarketplace.Endpoints    as Marketplace
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Marketplace
import qualified Plutus.Contracts.Services.Sale               as Sale
import           Plutus.PAB.Simulation                        (MarketplaceContracts (..))
import           Plutus.V1.Ledger.Time                        (DiffMilliSeconds)
import Plutus.Types.Marketplace as Marketplace

ratioBridge :: BridgePart
ratioBridge = do
  typeName ^== "Ratio"
  typeModule ^== "PlutusTx.Ratio"
  psRatio

psRatio :: MonadReader BridgeData m => m PSType
psRatio = expand <$> psTypeParameters
  where
    expand [x] = TypeInfo "web-common" "Data.Json.JsonTuple" "JsonTuple" [x, x]

marketplaceTypes :: [SumType 'Haskell]
marketplaceTypes =
      [ (equal <*> (genericShow <*> mkSumType)) (Proxy @ThreadToken)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @DiffMilliSeconds)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @MarketplaceContracts)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.Marketplace)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @(ContractResponse E A))
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.MarketplaceDatum)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.UserItemId)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.UserContractState)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.InfoContractState)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.NftInfo)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.NFT)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.Bundle)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.BundleInfo)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.NftBundle)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Auction.AuctionState)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Auction.HighestBid)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Sale.Sale)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.CreateNftParams)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.OpenSaleParams)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.CloseLotParams)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.StartAnAuctionParams)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.BidOnAuctionParams)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.BundleUpParams)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.UnbundleParams) ]
