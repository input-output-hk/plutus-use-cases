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
module Plutus.TestnetMVP.OffChain.Info where

import qualified Control.Lens                                    as Lens
import qualified Data.Aeson                                      as J
import           Data.Proxy                                      (Proxy (..))
import           Data.Text                                       (Text)
import qualified Data.Text                                       as T
import           Ext.Plutus.Ledger.Value                         (ChainIndexTxMap,
                                                                  utxosValue)
import           GHC.Generics                                    (Generic)
import qualified GHC.Generics                                    as Haskell
import           Ledger hiding (datumHash)
import           Ledger.Ada                                      (getLovelace)
import           Ledger.Typed.Tx
import           Plutus.Abstract.ContractResponse                (ContractResponse,
                                                                  withContractResponse)
import           Plutus.Abstract.Percentage                      (Percentage)
import           Plutus.Contract
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import qualified PlutusTx.AssocMap                               as AssocMap
import           PlutusTx.Prelude                                hiding
                                                                 (Semigroup (..))
import           Prelude                                         (Semigroup (..))
import qualified Prelude                                         as Haskell
import qualified Ledger.Tx as Tx
import qualified Data.Map  as Map
import qualified Plutus.V1.Ledger.Scripts as Scripts
import Plutus.TestnetMVP.OnChain.Script (Marketplace(..), MarketplaceDatum(..))
import Plutus.TestnetMVP.OnChain.Validator (marketplaceAddress)
import Plutus.TestnetMVP.OnChain.NFT (NFT)
import Plutus.TestnetMVP.OnChain.ID

-- | Gets current Marketplace store state
marketplaceStore :: Marketplace -> Contract w s Text MarketplaceDatum
marketplaceStore marketplace = do
  utxoTx <- utxosTxOutTxAt $ marketplaceAddress marketplace
  let datums = Tx._ciTxOutDatum . fst <$> Map.elems utxoTx
  getDatum datums
  where
    getDatum (x:[]) = case x of
        Left datumHash -> do
          mDatum <- datumFromHash datumHash
          dt <- maybe (throwError "Can't get data from datumHash") pure mDatum
          maybe (throwError "Can't deserialize datum") pure (PlutusTx.fromBuiltinData $ Scripts.getDatum dt)
        Right datum -> 
          maybe (throwError "Can't deserialize datum") pure $ PlutusTx.fromBuiltinData $ Scripts.getDatum datum
    getDatum [] = throwError "No datum on the marketplace address"
    getDatum _ = throwError "More than 1 datum on the marketplace address"

data MarketplaceSettingsInfo = MarketplaceSettingsInfo {
  msCreationFee :: Integer,
  msSaleFee     :: Percentage
}
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (J.ToJSON, J.FromJSON)

marketplaceSettings :: Marketplace -> Contract w s Text MarketplaceSettingsInfo
marketplaceSettings Marketplace {..} =
  pure MarketplaceSettingsInfo {
      msCreationFee = getLovelace marketplaceNFTFee,
      msSaleFee = marketplaceSaleFee
    }

getStateDatum ::
    Maybe (OnChainState MarketplaceDatum i, ChainIndexTxMap) -> Contract w s Text MarketplaceDatum
getStateDatum = maybe (throwError "Marketplace output not found") (pure . tyTxOutData . ocsTxOut . fst)

getNftEntry :: MarketplaceDatum -> InternalNftId -> Contract w s Text NFT
getNftEntry nftStore (InternalNftId ipfsCidHash _ipfsCid) =
        maybe (throwError "NFT has not been created") pure $
          AssocMap.lookup ipfsCidHash $ mdSingletons nftStore

-- | Gets all UTxOs belonging to a user and concats them into one Value
fundsAt :: PaymentPubKeyHash -> Contract w s Text Value
fundsAt pkh = utxosValue $ pubKeyHashAddress pkh Nothing

-- | Gets all UTxOs belonging to the Marketplace script and concats them into one Value
marketplaceFunds :: Marketplace -> Contract w s Text Value
marketplaceFunds marketplace = utxosValue $ marketplaceAddress marketplace

mapError' :: Contract w s SMContractError a -> Contract w s Text a
mapError' = mapError $ T.pack . Haskell.show

type MarketplaceInfoSchema =
    Endpoint "fundsAt" PaymentPubKeyHash
    .\/ Endpoint "marketplaceFunds" ()
    .\/ Endpoint "marketplaceStore" ()
    .\/ Endpoint "marketplaceSettings" ()

data InfoContractState =
    FundsAt Value
    | MarketplaceFunds Value
    | MarketplaceStore MarketplaceDatum
    | MarketplaceSettings MarketplaceSettingsInfo
    deriving stock (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON)

Lens.makeClassyPrisms ''InfoContractState

infoEndpoints :: Marketplace -> Promise (ContractResponse Haskell.String Text InfoContractState) MarketplaceInfoSchema Void ()
infoEndpoints marketplace =
    (withContractResponse (Proxy @"fundsAt") FundsAt fundsAt
    `select` withContractResponse (Proxy @"marketplaceFunds") MarketplaceFunds (const $ marketplaceFunds marketplace)
    `select` withContractResponse (Proxy @"marketplaceStore") MarketplaceStore (const $ marketplaceStore marketplace)
    `select` withContractResponse (Proxy @"marketplaceSettings") MarketplaceSettings (const $ marketplaceSettings marketplace)) <> infoEndpoints marketplace
