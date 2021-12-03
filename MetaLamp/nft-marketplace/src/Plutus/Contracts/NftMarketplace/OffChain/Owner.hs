{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
module Plutus.Contracts.NftMarketplace.OffChain.Owner where

import qualified Control.Lens                                 as Lens
import           Control.Monad                                hiding (fmap)
import qualified Data.Aeson                                   as J
import           Data.Proxy                                   (Proxy (..))
import           Data.Text                                    (Text)
import qualified Data.Text                                    as T
import           Ext.Plutus.Ledger.Index                      (minAdaTxOutValue)
import qualified GHC.Generics                                 as Haskell
import           Ledger
import           Ledger.Ada                                   (Ada (..))
import qualified Ledger.Typed.Scripts                         as Scripts
import           Ledger.Value
import           Plutus.Abstract.ContractResponse             (ContractResponse,
                                                               withContractResponse)
import           Plutus.Abstract.Percentage                   (Fractional,
                                                               mkPercentage)
import           Plutus.Abstract.RemoteData                   (RemoteData)
import           Plutus.Contract
import           Plutus.Contract.Request                      (ownPubKeyHash)
import           Plutus.Contract.StateMachine
import           Plutus.Contracts.Currency                    as Currency
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Core
import           Plutus.V1.Ledger.Ada                         (lovelaceValueOf)
import qualified PlutusTx
import qualified PlutusTx.AssocMap                            as AssocMap
import           PlutusTx.Prelude                             hiding
                                                              (Semigroup (..))
import           Prelude                                      (Semigroup (..))
import qualified Prelude                                      as Haskell
import qualified Schema
import           Text.Printf                                  (printf)

data StartMarketplaceParams = StartMarketplaceParams {
    creationFee :: Integer,  -- fee by minting and bundling
    saleFee     :: Fractional  -- fee by sale and auction
}
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

-- | Starts the NFT Marketplace protocol: minting protocol NFT, creating empty nft storage
start :: StartMarketplaceParams -> Contract w s Text Core.Marketplace
start StartMarketplaceParams {..} = do
    pkh <- ownPubKeyHash
    saleFeePercentage <- maybe (throwError "Operator's fee value should be in [0, 100]") pure $ mkPercentage saleFee
    let marketplace = Core.Marketplace pkh (Lovelace creationFee) saleFeePercentage
    let client = Core.marketplaceClient marketplace
    void $ mapError (T.pack . Haskell.show @SMContractError) $ runInitialise client (Core.MarketplaceDatum AssocMap.empty AssocMap.empty) minAdaTxOutValue

    logInfo @Haskell.String $ printf "started Marketplace %s at address %s" (Haskell.show marketplace) (Haskell.show $ Core.marketplaceAddress marketplace)
    pure marketplace

type MarketplaceOwnerSchema =
    Endpoint "start" StartMarketplaceParams

data OwnerContractState = Started Core.Marketplace
    deriving stock (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON)

Lens.makeClassyPrisms ''OwnerContractState

ownerEndpoints :: Promise (ContractResponse Haskell.String Text OwnerContractState) MarketplaceOwnerSchema Void ()
ownerEndpoints = withContractResponse (Proxy @"start") Started (start) <> ownerEndpoints
