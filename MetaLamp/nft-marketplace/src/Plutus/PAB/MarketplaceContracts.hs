{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Plutus.PAB.MarketplaceContracts where

import qualified Data.Aeson                                   as J
import qualified Data.OpenApi.Schema                          as OpenApi
import           Data.Text.Prettyprint.Doc                    (Pretty (..),
                                                               viaShow)
import           GHC.Generics                                 (Generic)
import           Plutus.Contract                              hiding (when)
import qualified Plutus.Contracts.NftMarketplace.Endpoints    as Marketplace
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Marketplace
import           Plutus.PAB.Effects.Contract.Builtin          (Builtin,
                                                               SomeBuiltin (..),
                                                               type (.\\))
import qualified Plutus.PAB.Effects.Contract.Builtin          as Builtin

data MarketplaceContracts =
    MarketplaceStart
    | MarketplaceInfo Marketplace.Marketplace
    | MarketplaceUser Marketplace.Marketplace
    | MarketplaceController Marketplace.Marketplace
    deriving (Eq, Show, Generic)
    deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

instance Pretty MarketplaceContracts where
    pretty = viaShow

instance Builtin.HasDefinitions MarketplaceContracts where
    getDefinitions = [MarketplaceStart]
    getSchema = \case
        MarketplaceUser _          -> Builtin.endpointsToSchemas @Marketplace.MarketplaceUserSchema
        MarketplaceInfo _          -> Builtin.endpointsToSchemas @Marketplace.MarketplaceInfoSchema
        MarketplaceStart           -> Builtin.endpointsToSchemas @Marketplace.MarketplaceOwnerSchema
        MarketplaceController _          -> Builtin.endpointsToSchemas @Marketplace.MarketplaceControllerSchema
    getContract = \case
        MarketplaceInfo marketplace       -> SomeBuiltin . awaitPromise $ Marketplace.infoEndpoints marketplace
        MarketplaceUser marketplace       -> SomeBuiltin . awaitPromise $ Marketplace.userEndpoints marketplace
        MarketplaceStart           -> SomeBuiltin . awaitPromise $ Marketplace.ownerEndpoints
        MarketplaceController marketplace       -> SomeBuiltin . awaitPromise $ Marketplace.controllerEndpoints marketplace
