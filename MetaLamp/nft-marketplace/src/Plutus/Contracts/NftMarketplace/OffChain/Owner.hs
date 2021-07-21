{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Plutus.Contracts.NftMarketplace.OffChain.Owner where

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
start :: () -> Contract w s Text Core.Marketplace
start () = start' $ do
    pkh <- pubKeyHash <$> ownPubKey
    fmap Currency.currencySymbol $
           mapError (T.pack . Haskell.show @Currency.CurrencyError) $
           Currency.forgeContract pkh [(Core.marketplaceProtocolName, 1)]

start' :: Contract w s Text CurrencySymbol -> Contract w s Text Core.Marketplace
start' getMarketplaceToken = do
    marketplaceToken <- getMarketplaceToken
    pkh <- pubKeyHash <$> ownPubKey
    let marketplace = Core.marketplace marketplaceToken
    let client = Core.marketplaceClient marketplace
    void $ mapError (T.pack . Haskell.show @SMContractError) $ runInitialise client (Core.MarketplaceDatum AssocMap.empty) mempty

    logInfo @Haskell.String $ printf "started Marketplace %s at address %s" (Haskell.show marketplace) (Haskell.show $ Core.marketplaceAddress marketplace)
    pure marketplace

type MarketplaceOwnerSchema =
    Endpoint "start" ()

data OwnerContractState = Started Core.Marketplace
    deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON)

ownerEndpoints :: Contract (ContractResponse Text OwnerContractState) MarketplaceOwnerSchema Void ()
ownerEndpoints = forever $ withContractResponse (Proxy @"start") Started start
