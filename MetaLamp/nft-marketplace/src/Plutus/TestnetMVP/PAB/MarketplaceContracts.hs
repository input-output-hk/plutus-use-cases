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

module Plutus.TestnetMVP.PAB.MarketplaceContracts where

import qualified Data.Aeson                                   as J
import qualified Data.OpenApi.Schema                          as OpenApi
import           Data.Text.Prettyprint.Doc                    (Pretty (..),
                                                               viaShow)
import           GHC.Generics                                 (Generic)
import           Plutus.Contract                             
import qualified Plutus.TestnetMVP.OffChain.Endpoints    as Marketplace
import qualified Plutus.TestnetMVP.OnChain.Script as Marketplace
import           Plutus.PAB.Effects.Contract.Builtin          (SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin          as Builtin
import           Plutus.PAB.Run.PSGenerator                   (HasPSTypes (..))

data MarketplaceContracts =
    MarketplaceStart
    | MarketplaceInfo Marketplace.Marketplace
    deriving (Eq, Show, Generic, Ord)
    deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

instance HasPSTypes MarketplaceContracts where
    psTypes = []

instance Pretty MarketplaceContracts where
    pretty = viaShow

instance Builtin.HasDefinitions MarketplaceContracts where
    getDefinitions = [MarketplaceStart]
    getSchema = \case
        -- MarketplaceUser _          -> Builtin.endpointsToSchemas @Marketplace.MarketplaceUserSchema
        MarketplaceInfo _          -> Builtin.endpointsToSchemas @Marketplace.MarketplaceInfoSchema
        MarketplaceStart           -> Builtin.endpointsToSchemas @Marketplace.MarketplaceOwnerSchema
    getContract = \case
        MarketplaceInfo marketplace       -> SomeBuiltin . awaitPromise $ Marketplace.infoEndpoints marketplace
        -- MarketplaceUser marketplace       -> SomeBuiltin . awaitPromise $ Marketplace.userEndpoints marketplace
        MarketplaceStart           -> SomeBuiltin . awaitPromise $ Marketplace.ownerEndpoints
