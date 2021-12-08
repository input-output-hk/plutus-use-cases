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
module Plutus.Contracts.NftMarketplace.OffChain.Info where

import           Control.Lens                                    (_2, _Left,
                                                                  _Right, (^.),
                                                                  (^?))
import qualified Control.Lens                                    as Lens
import           Control.Monad                                   hiding (fmap)
import qualified Data.Aeson                                      as J
import           Data.Proxy                                      (Proxy (..))
import           Data.Text                                       (Text)
import qualified Data.Text                                       as T
import           Ext.Plutus.Ledger.Value                         (ChainIndexTxMap,
                                                                  utxosValue)
import           GHC.Generics                                    (Generic)
import qualified GHC.Generics                                    as Haskell
import           Ledger
import           Ledger.Ada                                      (fromValue,
                                                                  getLovelace)
import qualified Ledger.Typed.Scripts                            as Scripts
import           Ledger.Typed.Tx
import           Ledger.Value
import           Plutus.Abstract.ContractResponse                (ContractResponse,
                                                                  withContractResponse)
import           Plutus.Abstract.Percentage                      (Percentage)
import           Plutus.Abstract.RemoteData                      (RemoteData)
import           Plutus.Contract
import           Plutus.Contract.StateMachine
import           Plutus.Contracts.NftMarketplace.OffChain.ID
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core    as Core
import           Plutus.Contracts.NftMarketplace.OnChain.Core.ID (InternalId (..))
import qualified PlutusTx
import qualified PlutusTx.AssocMap                               as AssocMap
import           PlutusTx.Prelude                                hiding
                                                                 (Semigroup (..))
import           Prelude                                         (Semigroup (..))
import qualified Prelude                                         as Haskell
import           Text.Printf                                     (printf)

-- | Gets current Marketplace store state
marketplaceStore :: Core.Marketplace -> Contract w s Text Core.MarketplaceDatum
marketplaceStore marketplace = do
  let client = Core.marketplaceClient marketplace
  mapError' (getOnChainState client) >>= getStateDatum

data MarketplaceSettingsInfo = MarketplaceSettingsInfo {
  msCreationFee :: Integer,
  msSaleFee     :: Percentage
}
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (J.ToJSON, J.FromJSON)

marketplaceSettings :: Core.Marketplace -> Contract w s Text MarketplaceSettingsInfo
marketplaceSettings Core.Marketplace {..} =
  pure MarketplaceSettingsInfo {
      msCreationFee = getLovelace marketplaceNFTFee,
      msSaleFee = marketplaceSaleFee
    }

getStateDatum ::
    Maybe (OnChainState Core.MarketplaceDatum i, ChainIndexTxMap) -> Contract w s Text Core.MarketplaceDatum
getStateDatum = maybe (throwError "Marketplace output not found") (pure . tyTxOutData . ocsTxOut . fst)

getNftEntry :: Core.MarketplaceDatum -> Core.InternalNftId -> Contract w s Text Core.NFT
getNftEntry nftStore (Core.InternalNftId ipfsCidHash ipfsCid) =
        maybe (throwError "NFT has not been created") pure $
          AssocMap.lookup ipfsCidHash $ Core.mdSingletons nftStore

-- | Gets all UTxOs belonging to a user and concats them into one Value
fundsAt :: PubKeyHash -> Contract w s Text Value
fundsAt pkh = utxosValue $ pubKeyHashAddress pkh

-- | Gets all UTxOs belonging to the Marketplace script and concats them into one Value
marketplaceFunds :: Core.Marketplace -> Contract w s Text Value
marketplaceFunds marketplace = utxosValue $ Core.marketplaceAddress marketplace

mapError' :: Contract w s SMContractError a -> Contract w s Text a
mapError' = mapError $ T.pack . Haskell.show

type MarketplaceInfoSchema =
    Endpoint "fundsAt" PubKeyHash
    .\/ Endpoint "marketplaceFunds" ()
    .\/ Endpoint "marketplaceStore" ()
    .\/ Endpoint "marketplaceSettings" ()

data InfoContractState =
    FundsAt Value
    | MarketplaceFunds Value
    | MarketplaceStore Core.MarketplaceDatum
    | MarketplaceSettings MarketplaceSettingsInfo
    deriving stock (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON)

Lens.makeClassyPrisms ''InfoContractState

infoEndpoints :: Core.Marketplace -> Promise (ContractResponse Haskell.String Text InfoContractState) MarketplaceInfoSchema Void ()
infoEndpoints marketplace =
    (withContractResponse (Proxy @"fundsAt") FundsAt fundsAt
    `select` withContractResponse (Proxy @"marketplaceFunds") MarketplaceFunds (const $ marketplaceFunds marketplace)
    `select` withContractResponse (Proxy @"marketplaceStore") MarketplaceStore (const $ marketplaceStore marketplace)
    `select` withContractResponse (Proxy @"marketplaceSettings") MarketplaceSettings (const $ marketplaceSettings marketplace)) <> infoEndpoints marketplace
