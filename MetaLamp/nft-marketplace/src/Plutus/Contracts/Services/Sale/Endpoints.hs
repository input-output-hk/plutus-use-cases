{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Plutus.Contracts.Services.Sale.Endpoints where

import           Control.Monad                               hiding (fmap)
import qualified Data.Aeson                                  as J
import           Data.Proxy                                  (Proxy (..))
import           Data.Text                                   (Text)
import qualified Data.Text                                   as T
import qualified GHC.Generics                                as Haskell
import           Ledger
import qualified Ledger.Typed.Scripts                        as Scripts
import           Ledger.Value
import           Plutus.Abstract.ContractResponse            (ContractResponse,
                                                              withContractResponse)
import           Plutus.Contract
import           Plutus.Contract.StateMachine
import           Plutus.Contracts.Currency                   as Currency
import qualified Plutus.Contracts.Services.Sale.Core         as Core
import qualified Plutus.Contracts.Services.Sale.StateMachine as Core
import qualified Plutus.Types.Marketplace                    as Marketplace
import qualified Plutus.Types.Percentage                     as Percentage

import qualified PlutusTx
import qualified PlutusTx.AssocMap                           as AssocMap
import           PlutusTx.Prelude                            hiding
                                                             (Semigroup (..))
import           Prelude                                     (Semigroup (..))
import qualified Prelude                                     as Haskell
import qualified Schema
import           Text.Printf                                 (printf)

data OpenSaleParams =
  OpenSaleParams {
    ospSalePrice :: Core.LovelacePrice,
    ospSaleValue :: Value
  }
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

PlutusTx.unstableMakeIsData ''OpenSaleParams
PlutusTx.makeLift ''OpenSaleParams

-- | Starts the Sale protocol and mints protocol NFT
openSale :: OpenSaleParams -> Marketplace.Marketplace -> Contract w s Text Core.Sale
openSale OpenSaleParams {..}  Marketplace.Marketplace {..} = do
    pkh <- getOwnPubKey
    saleToken <- mapError (T.pack . Haskell.show @SMContractError) $ getThreadToken
    let sale = Core.Sale
                { saleProtocolToken = saleToken,
                  salePrice         = ospSalePrice,
                  saleValue         = ospSaleValue,
                  saleOwner         = pkh,
                  marketplaceOperator = marketplaceOperator,
                  marketplaceSaleFee    = marketplaceSaleFee
                }
    let client = Core.saleClient sale
    void $ mapError (T.pack . Haskell.show @SMContractError) $ runInitialise client Core.SaleOngoing ospSaleValue

    logInfo @Haskell.String $ printf "Opened Sale %s at address %s" (Haskell.show sale) (Haskell.show $ Core.saleAddress sale)
    pure sale

-- | The user buys sale value paying sale price
buyLot :: Core.Sale -> Contract w s Text ()
buyLot sale = do
    pkh <- getOwnPubKey
    let client = Core.saleClient sale
    void $ mapError' $ runStep client $ Core.Buy pkh

    logInfo @Haskell.String $ printf "User %s bought lot from sale %s" (Haskell.show pkh) (Haskell.show sale)
    pure ()

-- | The user redeems sale value and sale protocol token
redeemLot :: Core.Sale -> Contract w s Text ()
redeemLot sale = do
    pkh <- getOwnPubKey
    let client = Core.saleClient sale
    void $ mapError' $ runStep client Core.Redeem

    logInfo @Haskell.String $ printf "User %s redeemed lot from sale %s" (Haskell.show pkh) (Haskell.show sale)
    pure ()

getOwnPubKey :: Contract w s Text PubKeyHash
getOwnPubKey = pubKeyHash <$> ownPubKey

mapError' :: Contract w s SMContractError a -> Contract w s Text a
mapError' = mapError $ T.pack . Haskell.show
