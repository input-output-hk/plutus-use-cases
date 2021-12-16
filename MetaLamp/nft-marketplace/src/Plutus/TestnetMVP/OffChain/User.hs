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
module Plutus.TestnetMVP.OffChain.User where

import qualified Control.Lens                                    as Lens
import           Control.Lens                                             ((?~), (.~))
import           Control.Monad                                   hiding (fmap)
import qualified Data.Aeson                                      as J
import           Data.Proxy                                      (Proxy (..))
import           Data.Text                                       (Text)
import qualified Data.Text                                       as T
import qualified GHC.Generics                                    as Haskell

import           Plutus.Abstract.ContractResponse                (ContractResponse,
                                                                  withContractResponse)
import           Plutus.Contract
import           Plutus.Contract.StateMachine
import qualified PlutusTx.AssocMap                               as AssocMap
import qualified Schema

import           PlutusTx.Prelude                                hiding
                                                                 (Semigroup (..))
import           Prelude                                         (Semigroup (..))
import qualified Prelude                                         as Haskell
import           Text.Printf                                     (printf)
import qualified Ledger.Tx as Tx
import qualified Ext.Plutus.Contracts.Currency as Currency
import qualified Plutus.AbstractTestnetMVP.TxUtils as TxUtils
import Plutus.TestnetMVP.OnChain.Script
import Plutus.TestnetMVP.OnChain.Validator
import Plutus.TestnetMVP.OffChain.Serialization (deserializeByteString, deserializePlutusBuiltinBS)
import           Ext.Plutus.Ledger.Index                                  (minAdaTxOutValue)
import qualified Ledger.Value                                             as V
import Plutus.TestnetMVP.OffChain.Info (marketplaceStore, marketplaceFunds, getNftEntry)
import qualified Data.Map as Map
import Plutus.AbstractTestnetMVP.OutputValue (OutputValue(..))
import qualified Plutus.TestnetMVP.Services.Sale.Endpoints as Sale
import Plutus.TestnetMVP.OffChain.ID
import Plutus.TestnetMVP.OnChain.ID
import Plutus.TestnetMVP.OnChain.NFT
import qualified Ledger.Constraints.TxConstraints       as Constraints
import Plutus.TestnetMVP.Services.Sale.Script (LovelacePrice)

data CreateNftParams =
  CreateNftParams {
    cnpIpfsCid        :: Text,
    cnpNftName        :: Text,
    cnpNftDescription :: Text,
    cnpNftCategory    :: [Text],
    cnpRevealIssuer   :: Bool
  }
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

Lens.makeClassy_ ''CreateNftParams

-- | The user specifies which NFT to mint and add to marketplace store,
--   he gets it into his wallet and the corresponding store entry is created
createNft :: Marketplace -> CreateNftParams -> Contract w s Text ()
createNft marketplace CreateNftParams {..} = do
    let ipfsCid = deserializeByteString cnpIpfsCid
    let ipfsCidHash = sha2_256 ipfsCid
    nftStore <- mdSingletons <$> marketplaceStore marketplace
    when (isJust $ AssocMap.lookup ipfsCidHash nftStore) $ throwError "Nft entry already exists"

    pkh <- ownPubKeyHash
    let nftName = V.TokenName ipfsCid
    nft <-
           mapError (T.pack . Haskell.show @Currency.CurrencyError) $
           Currency.mintContract pkh [(nftName, 1)]

    let nftEntry = NftInfo
            { niCurrency          = Currency.currencySymbol nft
            , niName        = deserializePlutusBuiltinBS cnpNftName
            , niDescription = deserializePlutusBuiltinBS cnpNftDescription
            , niCategory = deserializePlutusBuiltinBS <$> cnpNftCategory
            , niIssuer      = if cnpRevealIssuer then Just pkh else Nothing
            }

    newDatum <- insertNFT marketplace ipfsCidHash nftEntry
    outputValue <- makeOutputValue marketplace $ CreateNftRedeemer ipfsCidHash nftEntry
    value <- marketplaceFunds marketplace

    let tx = TxUtils.mustRoundTripToScript (marketplaceInstance marketplace) [outputValue] newDatum pkh value

    ledgerTx <- TxUtils.submitTxPair tx
    void $ awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx

    logInfo @Haskell.String $ printf "Created NFT %s with store entry %s" (Haskell.show nft) (Haskell.show nftEntry)
    pure ()

makeOutputValue :: Marketplace -> MarketplaceRedeemer -> Contract w s Text (OutputValue MarketplaceRedeemer)
makeOutputValue marketplace ovValue = do
  utxoTx <- utxosTxOutTxAt $ marketplaceAddress marketplace
  let utxoTxList = Map.toList utxoTx
  getUtxo utxoTxList
  where
    getUtxo [] = throwError "No utxo on the marketplace address"
    getUtxo ((ovOutRef, (ovOutTx, _)):[]) = pure OutputValue {..}
    getUtxo _ = throwError "More than 1 utxo on the marketplace address"

insertNFT :: Marketplace -> IpfsCidHash -> NftInfo -> Contract w s Text MarketplaceDatum
insertNFT marketplace ipfsCidHash nftRecord = do
    store@MarketplaceDatum{..} <- marketplaceStore marketplace
    let nftEntry = NFT {nftLot = Nothing, ..}
    pure $ store { mdSingletons = AssocMap.insert ipfsCidHash nftEntry mdSingletons}

insertNft' :: IpfsCidHash -> NFT -> MarketplaceDatum -> MarketplaceDatum
insertNft' ipfsCidHash nftEntry store@MarketplaceDatum{..}  = do
  store { mdSingletons = AssocMap.insert ipfsCidHash nftEntry mdSingletons}

data OpenSaleParams =
  OpenSaleParams {
    ospItemId    :: UserItemId,
    ospSalePrice :: LovelacePrice
  }
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

Lens.makeClassy_ ''OpenSaleParams

-- | The user opens sale for his NFT
openSale :: Marketplace -> OpenSaleParams -> Contract w s Text ()
openSale marketplace@Marketplace{..} OpenSaleParams {..} = do
    pkh <- ownPubKeyHash
    nftStore <- marketplaceStore marketplace

    let internalId = toInternalId ospItemId
        ipfsCid = getIpfsCid internalId
        ipfsCidHash = getIpfsCidHash internalId

    saleValue <- case internalId of
        NftInternalId nftId@(InternalNftId ipfsCidHash ipfsCid) ->
              nftValue ipfsCid <$> getNftEntry nftStore nftId

    let openSaleParams = Sale.OpenSaleParams {
                  Sale.ospIpfsCidHash = ipfsCidHash,
                  Sale.ospSalePrice = ospSalePrice,
                  Sale.ospSaleValue = saleValue
              }
    nftInStore <- maybe (traceError "NFT has not been created.") pure $ AssocMap.lookup ipfsCidHash $ mdSingletons nftStore
    sale <- Sale.openSale openSaleParams
    let lot = SaleLotLink sale
        newEntry = (_nftLot ?~ (ipfsCid, lot)) nftInStore
        newDatum = insertNft' ipfsCidHash newEntry nftStore

    outputValue <- makeOutputValue marketplace $ mkPutOnSaleRedeemer internalId lot
    value <- marketplaceFunds marketplace

    let tx = TxUtils.mustRoundTripToScript (marketplaceInstance marketplace) [outputValue] newDatum pkh value
    ledgerTx <- TxUtils.submitTxPair tx
    void $ awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx

    logInfo @Haskell.String $ printf "Created NFT sale %s" (Haskell.show sale)
    pure ()

data CloseLotParams =
  CloseLotParams {
    clpItemId    :: UserItemId
  }
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

Lens.makeClassy_ ''CloseLotParams

-- | The user buys specified NFT lot
buyItem :: Marketplace -> CloseLotParams -> Contract w s Text ()
buyItem marketplace CloseLotParams {..} = do
    let internalId = toInternalId clpItemId
    nftStore <- marketplaceStore marketplace
    pkh <- ownPubKeyHash
    let ipfsCidHash = getIpfsCidHash internalId

    nftInStore <- maybe (traceError "NFT has not been created.") pure $ AssocMap.lookup ipfsCidHash $ mdSingletons nftStore
    sale <- case internalId of
      NftInternalId nftId@(InternalNftId ipfsCidHash ipfsCid) -> do
        nftEntry <- getNftEntry nftStore nftId
        maybe (throwError "NFT has not been put on sale") pure $
            getSaleFromNFT nftEntry

    _ <- Sale.buyLot sale

    let lot = SaleLotLink sale
        newEntry = (_nftLot .~ Nothing) nftInStore
        newDatum = insertNft' ipfsCidHash newEntry nftStore



    outputValue <- makeOutputValue marketplace $ mkRemoveFromSaleRedeemer internalId
    value <- marketplaceFunds marketplace

    let tx = TxUtils.mustRoundTripToScript (marketplaceInstance marketplace) [outputValue] newDatum pkh value

    ledgerTx <- TxUtils.submitTxPair tx
    void $ awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx

    logInfo @Haskell.String $ printf "Bought lot from sale %s" (Haskell.show sale)
    pure ()

type MarketplaceUserSchema =
    Endpoint "createNft" CreateNftParams
    .\/ Endpoint "openSale" OpenSaleParams
    .\/ Endpoint "buyItem" CloseLotParams

data UserContractState =
    NftCreated
    | OpenedSale
    | NftBought
    deriving stock (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON)

Lens.makeClassyPrisms ''UserContractState

userEndpoints :: Marketplace -> Promise (ContractResponse Haskell.String Text UserContractState) MarketplaceUserSchema Void ()
userEndpoints marketplace =
    (withContractResponse (Proxy @"createNft") (const NftCreated) (createNft marketplace)
    `select` withContractResponse (Proxy @"openSale") (const OpenedSale) (openSale marketplace)
    `select` withContractResponse (Proxy @"buyItem") (const NftBought) (buyItem marketplace)) <>
    userEndpoints marketplace
