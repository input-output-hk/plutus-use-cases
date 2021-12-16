{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Control.Monad                                          (when)
import           Control.Monad.Reader                                   (MonadReader)
import           Data.Aeson.TypeScript.Internal
import           Data.Aeson.TypeScript.TH
import qualified Data.Aeson.Types                                       as Aeson
import           Data.ByteString                                        (ByteString)
import           Data.Proxy                                             (Proxy (Proxy))
import           Plutus.Abstract.ContractResponse                       (ContractState)
import qualified Plutus.Abstract.Percentage                             as Percentage
import           Plutus.Abstract.RemoteData                             (RemoteData)
import qualified Plutus.TestnetMVP.OffChain.Endpoints              as Marketplace
import qualified Plutus.TestnetMVP.OffChain.Serialization as Marketplace
import qualified Plutus.TestnetMVP.Services.Sale.Script                        as Sale
import           Plutus.PAB.MarketplaceContracts                        (MarketplaceContracts (..))
import           Plutus.V1.Ledger.Ada                                   (Ada)
import           Plutus.V1.Ledger.Crypto                                (PaymentPubKeyHash)
import           Plutus.V1.Ledger.Time                                  (DiffMilliSeconds,
                                                                         POSIXTime)
import           Plutus.V1.Ledger.Tx                                    (TxOutRef)
import           Plutus.V1.Ledger.TxId                                  (TxId)
import           Plutus.V1.Ledger.Value                                 (CurrencySymbol,
                                                                         TokenName,
                                                                         Value)
import qualified PlutusTx.AssocMap                                      as AssocMap
import           PlutusTx.Builtins.Internal                             (BuiltinByteString)
import           System.Directory                                       (doesDirectoryExist,
                                                                         removeDirectoryRecursive)

instance TypeScript BuiltinByteString where
  getTypeScriptType _ = "string"

instance TypeScript Marketplace.PlutusBuiltinByteString where
  getTypeScriptType _ = "string"

instance TypeScript POSIXTime where
  getTypeScriptType _ = "integer"

$(deriveTypeScript Aeson.defaultOptions ''TxOutRef)
$(deriveTypeScript Aeson.defaultOptions ''TxId)
$(deriveTypeScript Aeson.defaultOptions ''CurrencySymbol)
$(deriveTypeScript Aeson.defaultOptions ''PaymentPubKeyHash)
$(deriveTypeScript Aeson.defaultOptions ''Value)

instance (TypeScript a, TypeScript b) => TypeScript (AssocMap.Map a b) where
  getTypeScriptType _ = "AssocMap<" <> (getTypeScriptType (Proxy :: Proxy a)) <> ", " <> (getTypeScriptType (Proxy :: Proxy b)) <> ">"
  getTypeScriptDeclarations _ = [TSRawDeclaration "export type AssocMap<K, V> = [K, V][]"]

$(deriveTypeScript Aeson.defaultOptions ''TokenName)
$(deriveTypeScript Aeson.defaultOptions ''Ada)

-- TODO: write 'normally', if there is nothing else to do
instance (TypeScript a, TypeScript b) => TypeScript (RemoteData a b) where
  getTypeScriptType _ = "RemoteData<" <> (getTypeScriptType (Proxy :: Proxy a)) <> ", " <> (getTypeScriptType (Proxy :: Proxy b)) <> ">"
  getTypeScriptDeclarations _ = [
    TSRawDeclaration "export type RemoteData<E, A> = INotAsked | ILoading | IFailure<E> | ISuccess<A>;",
    TSRawDeclaration "export interface INotAsked { tag: \"NotAsked\"; }",
    TSRawDeclaration "export interface ILoading { tag: \"Loading\"; }",
    TSRawDeclaration "export interface IFailure<T> { tag: \"Failure\"; contents: T; }",
    TSRawDeclaration "export interface ISuccess<T> { tag: \"Success\"; contents: T; }"]

$(deriveTypeScript Aeson.defaultOptions ''DiffMilliSeconds)
$(deriveTypeScript Aeson.defaultOptions ''MarketplaceContracts)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.Marketplace)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.LotLink)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.MarketplaceDatum)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.UserItemId)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.UserContractState)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.InfoContractState)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.OwnerContractState)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.NftInfo)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.NFT)
$(deriveTypeScript Aeson.defaultOptions ''Sale.Sale)
$(deriveTypeScript Aeson.defaultOptions ''Percentage.Percentage)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.CreateNftParams)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.OpenSaleParams)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.CloseLotParams)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.MarketplaceSettingsInfo)
$(deriveTypeScript Aeson.defaultOptions ''ContractState)

formattingOptions :: FormattingOptions
formattingOptions = FormattingOptions
  { numIndentSpaces = 2
  , interfaceNameModifier = id
  , typeNameModifier = id
  , exportMode = ExportEach
  , typeAlternativesFormat = TypeAlias
  }

main :: IO ()
main = writeFile "generated.ts" $ formatTSDeclarations' formattingOptions (
    (getTypeScriptDeclarations (Proxy @DiffMilliSeconds)) <>
    (getTypeScriptDeclarations (Proxy @MarketplaceContracts)) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.Marketplace)) <>
    (getTypeScriptDeclarations (Proxy @(RemoteData T1 T2))) <>
    (getTypeScriptDeclarations (Proxy @(ContractState T1 T2 T3))) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.LotLink)) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.MarketplaceDatum)) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.UserItemId)) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.OwnerContractState)) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.UserContractState)) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.InfoContractState)) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.NftInfo)) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.NFT)) <>
    (getTypeScriptDeclarations (Proxy @Sale.Sale)) <>
    (getTypeScriptDeclarations (Proxy @Percentage.Percentage)) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.CreateNftParams)) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.OpenSaleParams)) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.CloseLotParams)) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.MarketplaceSettingsInfo)) <>
    (getTypeScriptDeclarations (Proxy @TxOutRef)) <>
    (getTypeScriptDeclarations (Proxy @TxId)) <>
    (getTypeScriptDeclarations (Proxy @CurrencySymbol)) <>
    (getTypeScriptDeclarations (Proxy @PaymentPubKeyHash)) <>
    (getTypeScriptDeclarations (Proxy @Value)) <>
    (getTypeScriptDeclarations (Proxy @TokenName)) <>
    (getTypeScriptDeclarations (Proxy @Ada)) <>
    (getTypeScriptDeclarations (Proxy @(AssocMap.Map T1 T2))) <>
    (getTypeScriptDeclarations (Proxy @(Either T1 T2))))
