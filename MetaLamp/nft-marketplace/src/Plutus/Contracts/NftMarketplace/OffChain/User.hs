{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Plutus.Contracts.NftMarketplace.OffChain.User where

import           Control.Lens                                             (_2,
                                                                           _Left,
                                                                           _Right,
                                                                           (^.),
                                                                           (^?))
import qualified Control.Lens                                             as Lens
import           Control.Monad                                            hiding
                                                                          (fmap)
import qualified Data.Aeson                                               as J
import           Data.Proxy                                               (Proxy (..))
import           Data.Text                                                (Text)
import qualified Data.Text                                                as T
import qualified GHC.Generics                                             as Haskell
import           Ledger
import qualified Ledger.Typed.Scripts                                     as Scripts
import           Ledger.Typed.Tx
import qualified Ledger.Value                                             as V
import           Plutus.Abstract.ContractResponse                         (ContractResponse,
                                                                           withContractResponse)
import           Plutus.Abstract.RemoteData                               (RemoteData)
import           Plutus.Contract
import           Plutus.Contract.Request                                  (ownPubKeyHash)
import           Plutus.Contract.StateMachine
import qualified Ext.Plutus.Contracts.Currency as Currency
import           Plutus.Contracts.NftMarketplace.OffChain.ID              (UserItemId (..),
                                                                           toInternalId)
import           Plutus.Contracts.NftMarketplace.OffChain.Info
import           Plutus.Contracts.NftMarketplace.OffChain.Serialization   (deserializeByteString,
                                                                           deserializePlutusBuiltinBS)
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core             as Core
import           Plutus.Contracts.NftMarketplace.OnChain.Core.ID          (InternalId (..))
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core.ID          as Core
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core.Marketplace as Marketplace
import qualified Plutus.Contracts.Services.Sale                           as Sale
import           Plutus.V1.Ledger.Time                                    (DiffMilliSeconds (..),
                                                                           POSIXTime (..),
                                                                           fromMilliSeconds)
import qualified PlutusTx
import qualified PlutusTx.AssocMap                                        as AssocMap
import           PlutusTx.Prelude                                         hiding
                                                                          (Semigroup (..))
import           Prelude                                                  (Semigroup (..))
import qualified Prelude                                                  as Haskell
import qualified Schema
import           Text.Printf                                              (printf)

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
createNft :: Core.Marketplace -> CreateNftParams -> Contract w s Text ()
createNft marketplace CreateNftParams {..} = do
    let ipfsCid = deserializeByteString cnpIpfsCid
    let ipfsCidHash = sha2_256 ipfsCid
    nftStore <- Core.mdSingletons <$> marketplaceStore marketplace
    when (isJust $ AssocMap.lookup ipfsCidHash nftStore) $ throwError "Nft entry already exists"

    pkh <- ownPubKeyHash
    let tokenName = V.TokenName ipfsCid
    nft <-
           mapError (T.pack . Haskell.show @Currency.CurrencyError) $
           Currency.mintContract pkh [(tokenName, 1)]
    let client = Core.marketplaceClient marketplace
    let nftEntry = Core.NftInfo
            { niCurrency          = Currency.currencySymbol nft
            , niName        = deserializePlutusBuiltinBS cnpNftName
            , niDescription = deserializePlutusBuiltinBS cnpNftDescription
            , niCategory = deserializePlutusBuiltinBS <$> cnpNftCategory
            , niIssuer      = if cnpRevealIssuer then Just pkh else Nothing
            }
    void $ mapError' $ runStep client $ Core.CreateNftRedeemer ipfsCidHash nftEntry

    logInfo @Haskell.String $ printf "Created NFT %s with store entry %s" (Haskell.show nft) (Haskell.show nftEntry)
    pure ()

data OpenSaleParams =
  OpenSaleParams {
    ospItemId    :: UserItemId,
    ospSalePrice :: Sale.LovelacePrice
  }
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

Lens.makeClassy_ ''OpenSaleParams

-- | The user opens sale for his NFT
openSale :: Core.Marketplace -> OpenSaleParams -> Contract w s Text ()
openSale marketplace@Core.Marketplace{..} OpenSaleParams {..} = do
    let internalId = toInternalId ospItemId
    nftStore <- marketplaceStore marketplace
    saleValue <- case internalId of
      NftInternalId nftId@(Core.InternalNftId ipfsCidHash ipfsCid) ->
        Core.nftValue ipfsCid <$> getNftEntry nftStore nftId
    let openSaleParams = Sale.OpenSaleParams {
                  ospSalePrice = ospSalePrice,
                  ospSaleValue = saleValue,
                  ospSaleFee = Just $ Sale.SaleFee marketplaceOperator marketplaceSaleFee
              }
    sale <- Sale.openSale openSaleParams

    let client = Core.marketplaceClient marketplace
    let lot = Core.SaleLotLink sale
    void $ mapError' $ runStep client $ Core.mkPutLotRedeemer internalId lot

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
buyItem :: Core.Marketplace -> CloseLotParams -> Contract w s Text ()
buyItem marketplace CloseLotParams {..} = do
    let internalId = toInternalId clpItemId
    nftStore <- marketplaceStore marketplace
    sale <- case internalId of
      NftInternalId nftId@(Core.InternalNftId ipfsCidHash ipfsCid) -> do
        nftEntry <- getNftEntry nftStore nftId
        maybe (throwError "NFT has not been put on sale") pure $
            Core.getSaleFromNFT nftEntry

    _ <- Sale.buyLot sale

    let client = Core.marketplaceClient marketplace
    void $ mapError' $ runStep client $ Core.mkRemoveLotRedeemer internalId

    logInfo @Haskell.String $ printf "Bought lot from sale %s" (Haskell.show sale)
    pure ()

-- | The user closes NFT sale and receives his token back
closeSale :: Core.Marketplace -> CloseLotParams -> Contract w s Text ()
closeSale marketplace CloseLotParams {..} = do
    let internalId = toInternalId clpItemId
    nftStore <- marketplaceStore marketplace
    sale <- case internalId of
      NftInternalId nftId@(Core.InternalNftId ipfsCidHash ipfsCid) -> do
        nftEntry <- getNftEntry nftStore nftId
        maybe (throwError "NFT has not been put on sale") pure $
            Core.getSaleFromNFT nftEntry

    _ <- Sale.redeemLot sale

    let client = Core.marketplaceClient marketplace
    void $ mapError' $ runStep client $ Core.mkRemoveLotRedeemer internalId

    logInfo @Haskell.String $ printf "Closed lot sale %s" (Haskell.show sale)
    pure ()

balanceAt :: PubKeyHash -> AssetClass -> Contract w s Text Integer
balanceAt pkh asset = flip V.assetClassValueOf asset <$> fundsAt pkh

ownPubKeyBalance :: Contract w s Text Value
ownPubKeyBalance = ownPubKeyHash >>= fundsAt

type MarketplaceUserSchema =
    Endpoint "createNft" CreateNftParams
    .\/ Endpoint "openSale" OpenSaleParams
    .\/ Endpoint "buyItem" CloseLotParams
    .\/ Endpoint "closeSale" CloseLotParams
    .\/ Endpoint "ownPubKey" ()
    .\/ Endpoint "ownPubKeyBalance" ()

data UserContractState =
    NftCreated
    | OpenedSale
    | NftBought
    | ClosedSale
    | GetPubKey PubKeyHash
    | GetPubKeyBalance Value
    deriving stock (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON)

Lens.makeClassyPrisms ''UserContractState

userEndpoints :: Core.Marketplace -> Promise (ContractResponse Haskell.String Text UserContractState) MarketplaceUserSchema Void ()
userEndpoints marketplace =
    (withContractResponse (Proxy @"createNft") (const NftCreated) (createNft marketplace)
    `select` withContractResponse (Proxy @"openSale") (const OpenedSale) (openSale marketplace)
    `select` withContractResponse (Proxy @"buyItem") (const NftBought) (buyItem marketplace)
    `select` withContractResponse (Proxy @"closeSale") (const ClosedSale) (closeSale marketplace)
    `select` withContractResponse (Proxy @"ownPubKey") GetPubKey (const ownPubKeyHash)
    `select` withContractResponse (Proxy @"ownPubKeyBalance") GetPubKeyBalance (const ownPubKeyBalance)) <> userEndpoints marketplace
