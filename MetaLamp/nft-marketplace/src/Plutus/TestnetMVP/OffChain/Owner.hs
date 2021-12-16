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
{-# LANGUAGE FlexibleContexts #-}
module Plutus.TestnetMVP.OffChain.Owner where

import qualified Control.Lens                                    as Lens
import           Control.Monad                                   hiding (fmap)
import qualified Data.Aeson                                      as J
import           Data.Proxy                                      (Proxy (..))
import           Data.Text                                       (Text)
import qualified Data.Text                                       as T
import qualified GHC.Generics                                    as Haskell

import           Ledger.Value
import           Plutus.Abstract.ContractResponse                (ContractResponse,
                                                                  withContractResponse)
import           Plutus.Contract
import           Plutus.Contract.StateMachine
import qualified PlutusTx.AssocMap                               as AssocMap
import qualified Schema
import           Ledger.Ada                                   (Ada (..))

import           PlutusTx.Prelude                                hiding
                                                                 (Semigroup (..))
import           Prelude                                         (Semigroup (..))
import qualified Prelude                                         as Haskell
import           Text.Printf                                     (printf)
import qualified Ledger.Tx as Tx
import qualified Ext.Plutus.Contracts.Currency as Currency
import qualified Plutus.AbstractTestnetMVP.TxUtils as TxUtils
import           Plutus.Abstract.Percentage                   (Fractional,
                                                               mkPercentage)
import Plutus.TestnetMVP.OnChain.Script
import Plutus.TestnetMVP.OnChain.Validator
import Plutus.TestnetMVP.OffChain.Serialization (deserializeByteString)
import           Ext.Plutus.Ledger.Index                                  (minAdaTxOutValue)
import qualified Ledger.Value                                             as V

-- imports for debug
import qualified Cardano.Api  as C
import Ledger.Tx.CardanoAPI (toCardanoAddress)
---

data StartMarketplaceParams = StartMarketplaceParams {
    marketplaceName :: T.Text,
    creationFee :: Integer,  -- fee by minting
    saleFee     :: Fractional  -- fee by sale
}
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

start :: StartMarketplaceParams -> Contract w s Text Marketplace
start StartMarketplaceParams{..} = do
    pkh <- ownPaymentPubKeyHash
    let marketplaceTokenName = V.TokenName $ deserializeByteString marketplaceName
    marketplaceCurrencySymbol <- fmap Currency.currencySymbol $
           mapError (T.pack . Haskell.show @Currency.CurrencyError) $
           Currency.mintContract pkh [(marketplaceTokenName, 1)]
    let marketplaceThreadToken = assetClass marketplaceCurrencySymbol marketplaceTokenName

    saleFeePercentage <- maybe (throwError "Operator's fee value should be in [0, 100]") pure $ mkPercentage saleFee
    let marketplace = 
            Marketplace {marketplaceOperator = pkh
                        , marketplaceThreadToken = marketplaceThreadToken
                        , marketplaceName = deserializeByteString marketplaceName
                        , marketplaceNFTFee = Lovelace creationFee
                        , marketplaceSaleFee = saleFeePercentage
                        }
        payment = assetClassValue marketplaceThreadToken 1 + minAdaTxOutValue
        datum = MarketplaceDatum AssocMap.empty
    
    let startMarketplaceTx = TxUtils.mustPayToScript (marketplaceInstance marketplace) pkh datum payment
    ledgerTx <- TxUtils.submitTxPair startMarketplaceTx
    void $ awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx

    -- TODO: remove Debug info or make NETWORK be configurable
    let realMarketplaceAddress = C.serialiseAddress <$> toCardanoAddress (C.Testnet $ C.NetworkMagic 1097911063) (marketplaceAddress marketplace)
    logInfo @Haskell.String $ printf "Marketplace address in testnet: %s" (Haskell.show realMarketplaceAddress)
    --

    logInfo @Haskell.String $ printf "Started Marketplace %s at address %s" (Haskell.show marketplace) (Haskell.show $ marketplaceAddress marketplace)
    pure marketplace

type MarketplaceOwnerSchema =
    Endpoint "start" StartMarketplaceParams

data OwnerContractState = 
    Started Marketplace
    deriving stock (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON)

Lens.makeClassyPrisms ''OwnerContractState

ownerEndpoints :: Promise (ContractResponse Haskell.String Text OwnerContractState) MarketplaceOwnerSchema Void ()
ownerEndpoints = withContractResponse (Proxy @"start") Started (start) <> ownerEndpoints
