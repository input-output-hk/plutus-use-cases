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

module Plutus.Contracts.NftMarketplace.OffChain.User where

import qualified Control.Lens                                  as Lens
import           Control.Monad                                 hiding (fmap)
import qualified Data.Aeson                                    as J
import           Data.Proxy                                    (Proxy (..))
import           Data.Text                                     (Text)
import qualified Data.Text                                     as T
import           Ext.Plutus.Ledger.Value                       (utxoValue)
import qualified GHC.Generics                                  as Haskell
import           Ledger
import qualified Ledger.Typed.Scripts                          as Scripts
import           Ledger.Typed.Tx
import qualified Ledger.Value                                  as V
import           Plutus.Abstract.ContractResponse              (ContractResponse,
                                                                withContractResponse)
import           Plutus.Contract
import           Plutus.Contract.StateMachine
import           Plutus.Contracts.Currency                     as Currency
import           Plutus.Contracts.NftMarketplace.OffChain.Info (fundsAt,
                                                                mapError',
                                                                marketplaceStore)
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core  as Core
import qualified Plutus.Contracts.Services.Sale                as Sale
import qualified PlutusTx
import qualified PlutusTx.AssocMap                             as AssocMap
import           PlutusTx.Prelude                              hiding
                                                               (Semigroup (..))
import           Prelude                                       (Semigroup (..))
import qualified Prelude                                       as Haskell
import qualified Schema
import           Text.Printf                                   (printf)

getOwnPubKey :: Contract w s Text PubKeyHash
getOwnPubKey = pubKeyHash <$> ownPubKey

data CreateNftParams =
  CreateNftParams {
    cnpIpfsCid        :: ByteString,
    cnpNftName        :: ByteString,
    cnpNftDescription :: ByteString,
    cnpRevealIssuer   :: Bool
  }
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

PlutusTx.unstableMakeIsData ''CreateNftParams
PlutusTx.makeLift ''CreateNftParams

-- | The user specifies which NFT to mint and add to marketplace store,
--   he gets it into his wallet and the corresponding store entry is created
createNft :: Core.Marketplace -> CreateNftParams -> Contract w s Text ()
createNft marketplace CreateNftParams {..} = do
    let ipfsCidHash = sha2_256 cnpIpfsCid
    nftStore <- marketplaceStore marketplace
    when (isJust $ AssocMap.lookup ipfsCidHash nftStore) $ throwError "Nft entry already exists"

    pkh <- getOwnPubKey
    let tokenName = V.TokenName cnpIpfsCid
    nft <-
           mapError (T.pack . Haskell.show @Currency.CurrencyError) $
           Currency.forgeContract pkh [(tokenName, 1)]

    let client = Core.marketplaceClient marketplace
    let nftEntry = Core.NFT
            { nftId          = Currency.currencySymbol nft
            , nftName        = cnpNftName
            , nftDescription = cnpNftDescription
            , nftIssuer      = if cnpRevealIssuer then Just pkh else Nothing
            , nftSale     = Nothing -- TODO validate that it's Nothing
            }
    void $ mapError' $ runStep client $ Core.CreateNftRedeemer ipfsCidHash nftEntry

    logInfo @Haskell.String $ printf "Created NFT %s with store entry %s" (Haskell.show nft) (Haskell.show nftEntry)
    pure ()

data OpenSaleParams =
  OpenSaleParams {
    ospIpfsCid   :: ByteString,
    ospSalePrice :: Sale.LovelacePrice
  }
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

PlutusTx.unstableMakeIsData ''OpenSaleParams
PlutusTx.makeLift ''OpenSaleParams

-- | The user
openSale :: Core.Marketplace -> OpenSaleParams -> Contract w s Text ()
openSale marketplace OpenSaleParams {..} = do
    let ipfsCidHash = sha2_256 ospIpfsCid
    nftStore <- marketplaceStore marketplace
    nftEntry <- maybe (throwError "NFT has not been created") pure $ AssocMap.lookup ipfsCidHash nftStore
    let tokenName = V.TokenName ospIpfsCid

    sale <- Sale.openSale
              Sale.OpenSaleParams {
                  ospSalePrice = ospSalePrice,
                  ospSaleValue = V.singleton (Core.nftId nftEntry) tokenName 1
              }

    let client = Core.marketplaceClient marketplace
    let lot = Core.Lot
                { lotSale          = Sale.toTuple sale
                , lotIpfsCid     = ospIpfsCid
                }
    void $ mapError' $ runStep client $ Core.OpenSaleRedeemer ipfsCidHash lot

    logInfo @Haskell.String $ printf "Created NFT sale %s" (Haskell.show lot)
    pure ()

balanceAt :: PubKeyHash -> AssetClass -> Contract w s Text Integer
balanceAt pkh asset = flip V.assetClassValueOf asset <$> fundsAt pkh

ownPubKeyBalance :: Contract w s Text Value
ownPubKeyBalance = getOwnPubKey >>= fundsAt

type MarketplaceUserSchema =
    Endpoint "createNft" CreateNftParams
    .\/ Endpoint "openSale" OpenSaleParams
    .\/ Endpoint "buyLot" Sale.Sale
    .\/ Endpoint "redeemLot" Sale.Sale
    .\/ Endpoint "ownPubKey" ()
    .\/ Endpoint "ownPubKeyBalance" ()

data UserContractState =
    NftCreated
    | OpenedSale
    | Bought
    | Redeemed
    | GetPubKey PubKeyHash
    | GetPubKeyBalance Value
    deriving stock (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON)

Lens.makeClassyPrisms ''UserContractState

userEndpoints :: Core.Marketplace -> Contract (ContractResponse Text UserContractState) MarketplaceUserSchema Void ()
userEndpoints marketplace = forever $
    withContractResponse (Proxy @"createNft") (const NftCreated) (createNft marketplace)
    `select` withContractResponse (Proxy @"openSale") (const OpenedSale) (openSale marketplace)
    `select` withContractResponse (Proxy @"buyLot") (const Bought) Sale.buyLot
    `select` withContractResponse (Proxy @"redeemLot") (const Redeemed) Sale.redeemLot
    `select` withContractResponse (Proxy @"ownPubKey") GetPubKey (const getOwnPubKey)
    `select` withContractResponse (Proxy @"ownPubKeyBalance") GetPubKeyBalance (const ownPubKeyBalance)
