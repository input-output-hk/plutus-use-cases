{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fobject-code #-}
module Plutus.TestnetMVP.OnChain.Validator where

import Plutus.TestnetMVP.OnChain.Script
import qualified Data.Aeson                                   as J
import qualified Schema
import qualified GHC.Generics                                 as Haskell
import qualified Prelude                                      as Haskell
import           Ledger.Value
import qualified PlutusTx
import qualified Ledger.Typed.Scripts                             as Scripts
import           PlutusTx.Prelude                                 
import           Plutus.Contracts.NftMarketplace.OnChain.Core.NFT (IpfsCidHash)
import Plutus.TestnetMVP.OnChain.NFT (getLotValue, nftValue, nftLot)
import qualified PlutusTx.AssocMap                               as AssocMap
import Plutus.V1.Ledger.Contexts (ScriptContext)
import Plutus.V1.Ledger.Address (Address)
import Ledger.Address (scriptAddress)
import Plutus.Contracts.NftMarketplace.OnChain.Core.ID (InternalNftId(..))
import Control.Monad (join)

newtype MarketplaceThreadToken = 
    MarketplaceThreadToken 
        {marketplaceProtocolInstance :: AssetClass}
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

PlutusTx.makeLift ''MarketplaceThreadToken

marketplaceInstance :: Marketplace -> Scripts.TypedValidator MarketplaceScript
marketplaceInstance marketplace = Scripts.mkTypedValidator @MarketplaceScript
    ($$(PlutusTx.compile [|| makeMarketplaceValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode marketplace)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @MarketplaceDatum @MarketplaceRedeemer

makeMarketplaceValidator :: Marketplace -> MarketplaceDatum -> MarketplaceRedeemer -> ScriptContext -> Bool
makeMarketplaceValidator marketplace datum (CreateNftRedeemer ipfsCidHash nftInfo) _ = trace "CreateNftRedeemer" $ validateCreateNft marketplace datum ipfsCidHash
makeMarketplaceValidator marketplace datum (PutOnSaleRedeemer val) _ = trace "PutOnSaleRedeemer" $ validatePutOnSale marketplace datum val
makeMarketplaceValidator marketplace datum (RemoveFromSaleRedeemer val) _ = trace "RemoveFromSaleRedeemer" $ validateRemoveFromSale marketplace datum val

validateCreateNft :: Marketplace -> MarketplaceDatum -> IpfsCidHash -> Bool
validateCreateNft marketplace MarketplaceDatum{..} ipfsCidHash =
    traceIfFalse "NFT is already in the marketplace" (isNothing $ AssocMap.lookup ipfsCidHash $ mdSingletons)

validatePutOnSale :: Marketplace -> MarketplaceDatum -> PutOnSaleRedeemerValue -> Bool
validatePutOnSale marketplace MarketplaceDatum {..} (PutNftLotRedeemer (InternalNftId ipfsCidHash ipfsCid) lot) =
    let mbNftEntry = AssocMap.lookup ipfsCidHash mdSingletons
        lotValue = getLotValue lot
        hasBeenPutOnSale = Just lotValue == (nftValue ipfsCid <$> mbNftEntry)
        isValidHash = sha2_256 ipfsCid == ipfsCidHash
        hasNoExistingLot = isNothing . join $ nftLot <$> mbNftEntry
    in  traceIfFalse "NFT not in the marketplace" (isJust mbNftEntry) &&
        traceIfFalse "NFT has not been put on sale" hasBeenPutOnSale &&
        traceIfFalse "Invalid IPFS Cid Hash" isValidHash &&
        traceIfFalse "NFT already has a lot" hasNoExistingLot

validateRemoveFromSale :: Marketplace -> MarketplaceDatum -> RemoveFromSaleRedeemerValue -> Bool
validateRemoveFromSale marketplace MarketplaceDatum{..} (RemoveNftLotRedeemer ipfsCidHash) =
    let mbNftEntry = AssocMap.lookup ipfsCidHash mdSingletons
        hasBeenPutOnSale = isJust . join $ nftLot <$> mbNftEntry
    in  traceIfFalse "NFT not in the marketplace" (isJust mbNftEntry) &&
        traceIfFalse "NFT has not been put on sale" hasBeenPutOnSale

marketplaceValidator :: Marketplace -> Scripts.Validator
marketplaceValidator = Scripts.validatorScript . marketplaceInstance

marketplaceAddress :: Marketplace -> Address
marketplaceAddress = scriptAddress . marketplaceValidator
