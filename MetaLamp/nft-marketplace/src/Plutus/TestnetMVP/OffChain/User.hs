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
import Plutus.Contracts.NftMarketplace.OffChain.Serialization (deserializeByteString, deserializePlutusBuiltinBS)
import           Ext.Plutus.Ledger.Index                                  (minAdaTxOutValue)
import qualified Ledger.Value                                             as V
import Plutus.TestnetMVP.OffChain.Info (marketplaceStore, marketplaceFunds)
import Plutus.Contracts.NftMarketplace.OnChain.Core.NFT
import qualified Data.Map as Map
import Plutus.AbstractTestnetMVP.OutputValue (OutputValue(..))

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

type MarketplaceUserSchema =
    Endpoint "createNft" CreateNftParams

data UserContractState =
    NftCreated
    deriving stock (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON)

Lens.makeClassyPrisms ''UserContractState

userEndpoints :: Marketplace -> Promise (ContractResponse Haskell.String Text UserContractState) MarketplaceUserSchema Void ()
userEndpoints marketplace =
    (withContractResponse (Proxy @"createNft") (const NftCreated) (createNft marketplace)) <> userEndpoints marketplace
