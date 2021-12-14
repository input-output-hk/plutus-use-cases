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

module Plutus.TestnetMVP.Services.Sale.Endpoints where

import           Control.Monad                                            hiding
                                                                          (fmap)
import qualified Data.Aeson                                               as J
import           Data.Proxy                                               (Proxy (..))
import           Data.Text                                                (Text)
import qualified Data.Text                                                as T
import qualified GHC.Generics                                             as Haskell
import           Ledger
import qualified Ledger.Typed.Scripts                                     as Scripts
import           Ledger.Value
import qualified Plutus.Abstract.Percentage                               as Percentage
import           Plutus.Contract
import           Plutus.Contract.StateMachine
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core.Marketplace as Marketplace
import qualified Plutus.Contracts.Services.Sale.Core                      as Core
import qualified Plutus.Contracts.Services.Sale.StateMachine              as Core

import           Ext.Plutus.Ledger.Index                                  (minAdaTxOutValue)
import           Plutus.Contract.Request                                  (ownPubKeyHash)
import qualified PlutusTx
import qualified PlutusTx.AssocMap                                        as AssocMap
import           PlutusTx.Prelude                                         hiding
                                                                          (Semigroup (..))
import           Prelude                                                  (Semigroup (..))
import qualified Prelude                                                  as Haskell
import qualified Schema
import           Text.Printf                                              (printf)
import Plutus.Contract.Request (currentTime)
import Plutus.Contracts.NftMarketplace.OnChain.Core.NFT (IpfsCidHash)
import Plutus.TestnetMVP.Services.Sale.Validator
import Plutus.TestnetMVP.Services.Sale.Script
import qualified Ledger.Value                                             as V
import qualified Ext.Plutus.Contracts.Currency as Currency
import qualified Plutus.AbstractTestnetMVP.TxUtils as TxUtils
import qualified Ledger.Tx as Tx
import Plutus.Contracts.NftMarketplace.OffChain.Serialization (deserializeByteString)
import Plutus.V1.Ledger.Ada (Ada(..), toValue)
import qualified Data.Map as Map
import Plutus.AbstractTestnetMVP.OutputValue (OutputValue(..))

-- imports for debug
import qualified Cardano.Api  as C
import Ledger.Tx.CardanoAPI (toCardanoAddress)
---

data OpenSaleParams =
  OpenSaleParams {
    ospIpfsCidHash :: IpfsCidHash,
    ospSalePrice :: Core.LovelacePrice,
    ospSaleValue :: Value
  }
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

PlutusTx.unstableMakeIsData ''OpenSaleParams
PlutusTx.makeLift ''OpenSaleParams

-- | Starts the Sale protocol and mints protocol NFT
openSale :: OpenSaleParams -> Contract w s Text Sale
openSale OpenSaleParams {..} = do
    pkh <- ownPubKeyHash

    curTime <- T.pack . Haskell.show <$> currentTime
    let saleTokenName = V.TokenName (ospIpfsCidHash <> deserializeByteString curTime)
    saleCurrencySymbol <- fmap Currency.currencySymbol $
           mapError (T.pack . Haskell.show @Currency.CurrencyError) $
           Currency.mintContract pkh [(saleTokenName, 1)]
    let saleToken = assetClass saleCurrencySymbol saleTokenName
    
    let sale = Sale
                { saleProtocolToken = saleToken,
                  salePrice         = ospSalePrice,
                  saleValue         = ospSaleValue,
                  saleOwner         = pkh
                }
        payment = assetClassValue saleToken 1 + ospSaleValue + minAdaTxOutValue
        datum = SaleOngoing

    let openSaleTx = TxUtils.mustPayToScript (saleInstance sale) pkh datum payment
    ledgerTx <- TxUtils.submitTxPair openSaleTx
    void $ awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx

    -- TODO: remove Debug info or make NETWORK be configurable
    let realSaleAddress = C.serialiseAddress <$> toCardanoAddress (C.Testnet $ C.NetworkMagic 1097911063) (saleAddress sale)
    logInfo @Haskell.String $ printf "Sale address in testnet: %s" (Haskell.show realSaleAddress)
    --

    logInfo @Haskell.String $ printf "Opened Sale %s at address %s" (Haskell.show sale) (Haskell.show $ saleAddress sale)
    pure sale

-- | The user buys sale value paying sale price
buyLot :: Sale -> Contract w s Text ()
buyLot sale@Sale{..} = do
    pkh <- ownPubKeyHash

    outputValue <- makeSaleOutputValue sale $ Buy pkh

    let tx = TxUtils.mustPayToPubKey (saleInstance sale) saleOwner (toValue . Lovelace $ salePrice)
              <> TxUtils.mustPayToScript (saleInstance sale) pkh SaleClosed minAdaTxOutValue
              <> TxUtils.mustSpendFromScript (saleInstance sale) [outputValue] pkh (saleValue + minAdaTxOutValue)
              

    ledgerTx <- TxUtils.submitTxPair tx
    void $ awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx

    logInfo @Haskell.String $ printf "User %s bought lot from sale %s" (Haskell.show pkh) (Haskell.show sale)
    pure ()

makeSaleOutputValue :: Sale -> SaleRedeemer -> Contract w s Text (OutputValue SaleRedeemer)
makeSaleOutputValue sale ovValue = do
  utxoTx <- utxosTxOutTxAt $ saleAddress sale
  let utxoTxList = Map.toList utxoTx
  getUtxo utxoTxList
  where
    getUtxo [] = throwError "No utxo on the sale address"
    getUtxo ((ovOutRef, (ovOutTx, _)):[]) = pure OutputValue {..}
    getUtxo _ = throwError "More than 1 utxo on the sale address"
