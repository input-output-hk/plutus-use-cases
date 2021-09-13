{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NoImplicitPrelude   #-}
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
import qualified GHC.Generics                                 as Haskell
import           Ledger
import qualified Ledger.Typed.Scripts                         as Scripts
import           Ledger.Value
import           Plutus.Abstract.ContractResponse             (ContractResponse,
                                                               withContractResponse)
import           Plutus.Contract
import           Plutus.Contract.StateMachine
import           Plutus.Contracts.Currency                    as Currency
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Core
import qualified PlutusTx
import qualified PlutusTx.AssocMap                            as AssocMap
import           PlutusTx.Prelude                             hiding
                                                              (Semigroup (..))
import           Prelude                                      (Semigroup (..))
import qualified Prelude                                      as Haskell
import           Text.Printf                                  (printf)

-- | Starts the NFT Marketplace protocol: minting protocol NFT, creating empty nft storage
start :: Contract w s Text Core.Marketplace
start = do
    pkh <- pubKeyHash <$> ownPubKey
    let marketplace = Core.Marketplace pkh
    let client = Core.marketplaceClient marketplace
    void $ mapError (T.pack . Haskell.show @SMContractError) $ runInitialise client (Core.MarketplaceDatum AssocMap.empty AssocMap.empty) mempty

    logInfo @Haskell.String $ printf "started Marketplace %s at address %s" (Haskell.show marketplace) (Haskell.show $ Core.marketplaceAddress marketplace)
    pure marketplace

type MarketplaceOwnerSchema =
    Endpoint "start" ()

data OwnerContractState = Started Core.Marketplace
    deriving stock (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON)

Lens.makeClassyPrisms ''OwnerContractState

ownerEndpoints :: Promise (ContractResponse Text OwnerContractState) MarketplaceOwnerSchema Void ()
ownerEndpoints = withContractResponse (Proxy @"start") Started (const start) <> ownerEndpoints
