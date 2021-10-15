{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell          #-}

module Main where

import           Control.Monad              (when)
import           System.Directory           (doesDirectoryExist,
                                             removeDirectoryRecursive)
import           Control.Monad.Reader                         (MonadReader)
import           Data.Proxy                                   (Proxy (Proxy))
import qualified Ext.Plutus.Contracts.Auction                 as Auction
import           Plutus.Abstract.RemoteData                   (RemoteData)
import           Plutus.Contract.StateMachine.ThreadToken     (ThreadToken)
import qualified Plutus.Contracts.NftMarketplace.Endpoints    as Marketplace
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Marketplace
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core.NFT as NFT
import qualified Plutus.Contracts.Services.Sale               as Sale
import           Plutus.PAB.Simulation                        (MarketplaceContracts (..))
import           Plutus.V1.Ledger.Time                        (DiffMilliSeconds)
import Data.Aeson.TypeScript.TH
import Data.Aeson.TypeScript.Internal
import qualified Data.Aeson.Types as Aeson
import qualified Plutus.Abstract.Percentage as Percentage
import Plutus.V1.Ledger.Tx (TxOutRef)
import Plutus.V1.Ledger.TxId (TxId)
import PlutusTx.Builtins.Internal (BuiltinByteString)
import Data.ByteString (ByteString)
import Plutus.V1.Ledger.Value (CurrencySymbol, Value, TokenName)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import qualified PlutusTx.AssocMap as AssocMap
import Plutus.V1.Ledger.Ada (Ada)
import qualified Ext.Plutus.Contracts.Auction                     as Auction

instance TypeScript BuiltinByteString where
  getTypeScriptType _ = "string"

$(deriveTypeScript Aeson.defaultOptions ''TxOutRef)
$(deriveTypeScript Aeson.defaultOptions ''TxId)
$(deriveTypeScript Aeson.defaultOptions ''CurrencySymbol)
$(deriveTypeScript Aeson.defaultOptions ''PubKeyHash)
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

$(deriveTypeScript Aeson.defaultOptions ''ThreadToken)
$(deriveTypeScript Aeson.defaultOptions ''DiffMilliSeconds)
$(deriveTypeScript Aeson.defaultOptions ''MarketplaceContracts)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.Marketplace)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.MarketplaceDatum)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.UserItemId)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.UserContractState)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.InfoContractState)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.OwnerContractState)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.NftInfo)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.NFT)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.Bundle)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.BundleInfo)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.NftBundle)
$(deriveTypeScript Aeson.defaultOptions ''NFT.Auction)
$(deriveTypeScript Aeson.defaultOptions ''Auction.AuctionState)
$(deriveTypeScript Aeson.defaultOptions ''Auction.HighestBid)
$(deriveTypeScript Aeson.defaultOptions ''Auction.AuctionFee)
$(deriveTypeScript Aeson.defaultOptions ''Sale.Sale)
$(deriveTypeScript Aeson.defaultOptions ''Sale.SaleFee)
$(deriveTypeScript Aeson.defaultOptions ''Percentage.Percentage)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.CreateNftParams)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.OpenSaleParams)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.CloseLotParams)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.StartAnAuctionParams)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.BidOnAuctionParams)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.BundleUpParams)
$(deriveTypeScript Aeson.defaultOptions ''Marketplace.UnbundleParams)


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
    (getTypeScriptDeclarations (Proxy @ThreadToken)) <>
    (getTypeScriptDeclarations (Proxy @DiffMilliSeconds)) <>
    (getTypeScriptDeclarations (Proxy @MarketplaceContracts)) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.Marketplace)) <>
    (getTypeScriptDeclarations (Proxy @(RemoteData T1 T2))) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.MarketplaceDatum)) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.UserItemId)) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.OwnerContractState)) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.UserContractState)) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.InfoContractState)) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.NftInfo)) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.NFT)) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.Bundle)) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.BundleInfo)) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.NftBundle)) <>
    (getTypeScriptDeclarations (Proxy @NFT.Auction)) <>
    (getTypeScriptDeclarations (Proxy @Auction.AuctionState)) <>
    (getTypeScriptDeclarations (Proxy @Auction.HighestBid)) <>
    (getTypeScriptDeclarations (Proxy @Auction.AuctionFee)) <>
    (getTypeScriptDeclarations (Proxy @Sale.Sale)) <>
    (getTypeScriptDeclarations (Proxy @Sale.SaleFee)) <>
    (getTypeScriptDeclarations (Proxy @Percentage.Percentage)) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.CreateNftParams)) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.OpenSaleParams)) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.CloseLotParams)) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.StartAnAuctionParams)) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.BidOnAuctionParams)) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.BundleUpParams)) <>
    (getTypeScriptDeclarations (Proxy @Marketplace.UnbundleParams)) <>
    (getTypeScriptDeclarations (Proxy @TxOutRef)) <>
    (getTypeScriptDeclarations (Proxy @TxId)) <>
    (getTypeScriptDeclarations (Proxy @CurrencySymbol)) <>
    (getTypeScriptDeclarations (Proxy @PubKeyHash)) <>
    (getTypeScriptDeclarations (Proxy @Value)) <>
    (getTypeScriptDeclarations (Proxy @TokenName)) <>
    (getTypeScriptDeclarations (Proxy @Ada)) <>
    (getTypeScriptDeclarations (Proxy @(AssocMap.Map T1 T2))) <>
    (getTypeScriptDeclarations (Proxy @(Either T1 T2))))
