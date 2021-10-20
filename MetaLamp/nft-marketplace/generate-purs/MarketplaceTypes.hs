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
import qualified Plutus.Abstract.Percentage                   as Percentage
import           Plutus.Abstract.RemoteData                   (RemoteData)
import           Plutus.Contract.StateMachine.ThreadToken     (ThreadToken)
import qualified Plutus.Contracts.NftMarketplace.Endpoints    as Marketplace
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Marketplace
import qualified Plutus.Contracts.Services.Auction            as Auction
import qualified Plutus.Contracts.Services.Sale               as Sale
import           Plutus.PAB.MarketplaceContracts                       (MarketplaceContracts (..))

psPosixTime :: MonadReader BridgeData m => m PSType
psPosixTime =
    TypeInfo "purescript-foreign-generic" "Plutus.V1.Ledger.Time" "POSIXTime" <$>
    psTypeParameters

posixTimeBridge :: BridgePart
posixTimeBridge = do
    typeName ^== "POSIXTime"
    typeModule ^== "Plutus.V1.Ledger.Time"
    psPosixTime

marketplaceTypes :: [SumType 'Haskell]
marketplaceTypes =
      [ (equal <*> (genericShow <*> mkSumType)) (Proxy @ThreadToken)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @MarketplaceContracts)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Percentage.Percentage)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.Marketplace)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @(RemoteData E A))
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.MarketplaceDatum)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.UserItemId)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.UserContractState)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.InfoContractState)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.NftInfo)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.LotLink)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.NFT)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.Bundle)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.BundleInfo)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.NftBundle)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Auction.AuctionFee)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Auction.AuctionState)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Auction.HighestBid)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Sale.Sale)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Sale.SaleFee)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Auction.Auction)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.CreateNftParams)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.OpenSaleParams)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.CloseLotParams)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.StartAnAuctionParams)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.BidOnAuctionParams)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.BundleUpParams)
          , (equal <*> (genericShow <*> mkSumType)) (Proxy @Marketplace.UnbundleParams) ]
