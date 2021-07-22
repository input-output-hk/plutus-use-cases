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
import           Ledger.Value
import           Plutus.Abstract.ContractResponse              (ContractResponse,
                                                                withContractResponse)
import           Plutus.Contract
import           Plutus.Contract.StateMachine
import           Plutus.Contracts.Currency                     as Currency
import           Plutus.Contracts.NftMarketplace.OffChain.Info (fundsAt,
                                                                mapError',
                                                                marketplaceStore)
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core  as Core
import qualified PlutusTx
import qualified PlutusTx.AssocMap                             as AssocMap
import           PlutusTx.Prelude                              hiding
                                                               (Semigroup (..))
import           Prelude                                       (Semigroup (..))
import qualified Prelude                                       as Haskell
import qualified Schema
import           Text.Printf                                   (printf)

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

-- | The user specifizes which NFT to mint and add to marketplace store,
--   he gets it into his wallet and the corresponding store entry is created
createNft :: Core.Marketplace -> CreateNftParams -> Contract w s Text ()
createNft marketplace CreateNftParams {..} = do
    let ipfsCidHash = sha2_256 cnpIpfsCid
    nftStore <- marketplaceStore marketplace
    when (isJust $ AssocMap.lookup ipfsCidHash nftStore) $ throwError "Nft entry already exists"

    pkh <- getOwnPubKey
    let tokenName = TokenName cnpIpfsCid
    nft <-
           mapError (T.pack . Haskell.show @Currency.CurrencyError) $
           Currency.forgeContract pkh [(tokenName, 1)]

    let client = Core.marketplaceClient marketplace
    let nftEntry = Core.NFT
            { nftId          = Currency.currencySymbol nft
            , nftName        = cnpNftName
            , nftDescription = cnpNftDescription
            , nftIssuer      = if cnpRevealIssuer then Just pkh else Nothing
            , nftIpfsCid     = Nothing
            }
    void $ mapError' $ runStep client $ Core.CreateNftRedeemer ipfsCidHash nftEntry

    logInfo @Haskell.String $ printf "Created NFT %s with store entry %s" (Haskell.show nft) (Haskell.show nftEntry)
    pure ()

balanceAt :: PubKeyHash -> AssetClass -> Contract w s Text Integer
balanceAt pkh asset = flip assetClassValueOf asset <$> fundsAt pkh

getOwnPubKey :: Contract w s Text PubKeyHash
getOwnPubKey = pubKeyHash <$> ownPubKey

ownPubKeyBalance :: Contract w s Text Value
ownPubKeyBalance = getOwnPubKey >>= fundsAt

type MarketplaceUserSchema =
    Endpoint "createNft" CreateNftParams
    .\/ Endpoint "ownPubKey" ()
    .\/ Endpoint "ownPubKeyBalance" ()

data UserContractState =
    NftCreated
    | GetPubKey PubKeyHash
    | GetPubKeyBalance Value
    deriving stock (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON)

Lens.makeClassyPrisms ''UserContractState

userEndpoints :: Core.Marketplace -> Contract (ContractResponse Text UserContractState) MarketplaceUserSchema Void ()
userEndpoints marketplace = forever $
    withContractResponse (Proxy @"createNft") (const NftCreated) (createNft marketplace)
    `select` withContractResponse (Proxy @"ownPubKey") GetPubKey (const getOwnPubKey)
    `select` withContractResponse (Proxy @"ownPubKeyBalance") GetPubKeyBalance (const ownPubKeyBalance)
