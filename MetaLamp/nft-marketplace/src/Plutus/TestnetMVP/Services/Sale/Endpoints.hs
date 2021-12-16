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
import Plutus.TestnetMVP.OnChain.NFT (IpfsCidHash)
import Plutus.TestnetMVP.Services.Sale.Validator
import Plutus.TestnetMVP.Services.Sale.Script
import qualified Ledger.Value                                             as V
import qualified Ext.Plutus.Contracts.Currency as Currency
import qualified Plutus.AbstractTestnetMVP.TxUtils as TxUtils
import qualified Ledger.Tx as Tx
import Plutus.TestnetMVP.OffChain.Serialization (deserializeByteString)
import Plutus.V1.Ledger.Ada (Ada(..), toValue)
import qualified Data.Map as Map
import Plutus.AbstractTestnetMVP.OutputValue (OutputValue(..))
import qualified Ledger.Constraints.TxConstraints as TxConstraints
import qualified Ledger.Constraints.OffChain as TxConstraints
import           Ext.Plutus.Ledger.Value (utxosValue)
import Ledger.Tokens (token)
import Plutus.V1.Ledger.Address (pubKeyHashAddress)
import Plutus.TestnetMVP.OffChain.Info (fundsAt)
-- imports for debug
import qualified Cardano.Api  as C
import Ledger.Tx.CardanoAPI (toCardanoAddress)
---

data OpenSaleParams =
  OpenSaleParams {
    ospIpfsCidHash :: IpfsCidHash,
    ospSalePrice :: LovelacePrice,
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
        payment = assetClassValue saleToken 1 + ospSaleValue + minAdaTxOutValue + minAdaTxOutValue
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
    buyer <- ownPubKeyHash

    utxoPair <- getUtxoPair sale
    value <- utxosValue $ saleAddress sale

    let outputValue = makeSaleOutputValue utxoPair $ Buy buyer

    logInfo @Haskell.String $ printf "[BUY_LOT] value on sale script address %s" (Haskell.show value)
    logInfo @Haskell.String $ printf "[BUY_LOT] SALE value %s" (Haskell.show saleValue)

    let ref = saleTxOutRef utxoPair
    let scriptUtxo = getUtxoMap utxoPair
    buyerUtxos <- utxosAt $ pubKeyHashAddress buyer

    -- pay to seller Tx
    let toSeller = TxUtils.mustPayToPubKey (saleInstance sale) saleOwner (toValue . Lovelace $ salePrice)
    toSellerLedgerTx <- TxUtils.submitTxPair toSeller
    void $ awaitTxConfirmed $ Tx.getCardanoTxId toSellerLedgerTx

    -- spend token from script and send it to buyer
    let lookups = TxConstraints.unspentOutputs scriptUtxo
                  <> TxConstraints.otherScript (saleValidator sale) 
                  <> TxConstraints.typedValidatorLookups (saleInstance sale)
    let tx =    TxConstraints.mustSpendScriptOutput ref (Redeemer . PlutusTx.toBuiltinData $ Buy buyer) 
                <> TxConstraints.mustPayToPubKey buyer (saleValue + minAdaTxOutValue)
                <> TxConstraints.mustPayToTheScript SaleClosed (token saleProtocolToken + minAdaTxOutValue)
    ledgerTx <- submitTxConstraintsWith @SaleScript lookups tx
    void $ awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx

    value1 <- utxosValue $ saleAddress sale 
    logInfo @Haskell.String $ printf "[BUY_LOT] value after buy on sale script address %s" (Haskell.show value1)
    buyerFunds <- fundsAt buyer
    logInfo @Haskell.String $ printf "[BUY_LOT] final buyer funds %s" (Haskell.show buyerFunds)
    sellerFunds <- fundsAt saleOwner
    logInfo @Haskell.String $ printf "[BUY_LOT] final sale owner funds %s" (Haskell.show sellerFunds)
    logInfo @Haskell.String $ printf "User %s bought lot from sale %s" (Haskell.show buyer) (Haskell.show sale)
    pure ()

saleTxOutRef :: (TxOutRef, ChainIndexTxOut) -> TxOutRef
saleTxOutRef (txOutRef, _) = txOutRef

makeSaleOutputValue :: (TxOutRef, ChainIndexTxOut) -> SaleRedeemer -> OutputValue SaleRedeemer
makeSaleOutputValue (ovOutRef ,ovOutTx) ovValue = OutputValue {..}

getUtxoMap :: (TxOutRef, ChainIndexTxOut) -> Map.Map TxOutRef ChainIndexTxOut
getUtxoMap (txOutRef, chainIndexTxOut) = Map.singleton txOutRef chainIndexTxOut

getUtxoPair :: Sale -> Contract w s Text (TxOutRef, ChainIndexTxOut)
getUtxoPair sale = do
  utxoTx <- utxosTxOutTxAt $ saleAddress sale
  let utxoTxList = Map.toList utxoTx
  getUtxo utxoTxList
  where
    getUtxo [] = throwError "No utxo on the sale address"
    getUtxo ((ovOutRef, (ovOutTx, _)):[]) = pure (ovOutRef, ovOutTx)
    getUtxo _ = throwError "More than 1 utxo on the sale address"

