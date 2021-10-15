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
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fobject-code #-}

module Plutus.Contracts.NftMarketplace.OnChain.Core.NFT where

import           Control.Lens                           ((&), (.~), (?~), (^?), (^.), _2)
import qualified Control.Lens                           as Lens
import qualified Crypto.Hash                            as Hash
import qualified Data.Aeson                             as J
import qualified Data.ByteArray                         as BA
import qualified Data.List                              as HL
import qualified Data.Text                              as T
import qualified GHC.Generics                           as Haskell
import           Ledger
import qualified Ledger.Constraints                     as Constraints
import qualified Ledger.Typed.Scripts                   as Scripts
import qualified Ledger.Value                           as V
import           Plutus.Abstract.Percentage             (Percentage)
import           Plutus.Contract
import           Plutus.Contract.StateMachine
import qualified Plutus.Contract.StateMachine           as SM
import           Plutus.Contracts.Services.Auction.Core (Auction (..),
                                                         AuctionFee)
import qualified Plutus.Contracts.Services.Auction.Core as Auction
import qualified Plutus.Contracts.Services.Sale         as Sale
import qualified PlutusTx
import qualified PlutusTx.AssocMap                      as AssocMap
import           PlutusTx.Prelude                       hiding (Semigroup (..))
import           Prelude                                (Semigroup (..))
import qualified Prelude                                as Haskell
import           Control.Monad          (join)

-- Category = [BuiltinByteString]
-- 1. acts as a list of category with nested subcategories
-- 2. acts as a list of tags
type IpfsCid = BuiltinByteString
type IpfsCidHash = BuiltinByteString
type Category = [BuiltinByteString]
type BundleId = BuiltinByteString

data LotLink = 
  SaleLotLink Sale.Sale 
  | AuctionLotLink Auction.Auction
  deriving stock (Haskell.Eq, Haskell.Show, Haskell.Generic)
  deriving anyclass (J.ToJSON, J.FromJSON)

PlutusTx.unstableMakeIsData ''LotLink

PlutusTx.makeLift ''LotLink

Lens.makeClassy_ ''LotLink

getSaleFromLot :: LotLink -> Maybe Sale.Sale
getSaleFromLot (SaleLotLink sale) = Just sale
getSaleFromLot _ = Nothing

getAuctionFromLot :: LotLink -> Maybe Auction.Auction
getAuctionFromLot (AuctionLotLink auction) = Just auction
getAuctionFromLot _ = Nothing

getLotValue :: LotLink -> V.Value
getLotValue (SaleLotLink sale) = Sale.saleValue sale 
getLotValue (AuctionLotLink auction) = Auction.aAsset auction

data NftInfo =
  NftInfo
    { niCurrency    :: !CurrencySymbol
    , niName        :: !BuiltinByteString
    , niDescription :: !BuiltinByteString
    , niCategory    :: !Category
    , niIssuer      :: !(Maybe PubKeyHash)
    }
  deriving stock (Haskell.Eq, Haskell.Show, Haskell.Generic)
  deriving anyclass (J.ToJSON, J.FromJSON)

PlutusTx.unstableMakeIsData ''NftInfo

PlutusTx.makeLift ''NftInfo

Lens.makeClassy_ ''NftInfo

data NFT =
  NFT
    { nftRecord :: !NftInfo
    , nftLot    :: !(Maybe (IpfsCid, LotLink))
    }
  deriving stock (Haskell.Eq, Haskell.Show, Haskell.Generic)
  deriving anyclass (J.ToJSON, J.FromJSON)

PlutusTx.unstableMakeIsData ''NFT

PlutusTx.makeLift ''NFT

Lens.makeClassy_ ''NFT

getAuctionFromNFT :: NFT -> Maybe Auction.Auction
getAuctionFromNFT nft = join (getAuctionFromLot <$> (nft ^. _nftLot ^? traverse . _2))

getSaleFromNFT :: NFT -> Maybe Sale.Sale
getSaleFromNFT nft = join (getSaleFromLot <$> (nft ^. _nftLot ^? traverse . _2))

data Bundle
  = NoLot  !(AssocMap.Map IpfsCidHash NftInfo)
  | HasLot !(AssocMap.Map IpfsCidHash (IpfsCid, NftInfo)) !LotLink
  deriving stock (Haskell.Eq, Haskell.Show, Haskell.Generic)
  deriving anyclass (J.ToJSON, J.FromJSON)

PlutusTx.unstableMakeIsData ''Bundle

PlutusTx.makeLift ''Bundle

Lens.makeClassyPrisms ''Bundle

data BundleInfo =
  BundleInfo
    { biName        :: !BuiltinByteString
    , biDescription :: !BuiltinByteString
    , biCategory    :: !Category
    }
  deriving stock (Haskell.Eq, Haskell.Show, Haskell.Generic)
  deriving anyclass (J.ToJSON, J.FromJSON)

PlutusTx.unstableMakeIsData ''BundleInfo

PlutusTx.makeLift ''BundleInfo

Lens.makeClassy_ ''BundleInfo

data NftBundle =
  NftBundle
    { nbRecord :: !BundleInfo
    , nbTokens :: !Bundle
    }
  deriving stock (Haskell.Eq, Haskell.Show, Haskell.Generic)
  deriving anyclass (J.ToJSON, J.FromJSON)

PlutusTx.unstableMakeIsData ''NftBundle

PlutusTx.makeLift ''NftBundle

Lens.makeClassy_ ''NftBundle

getAuctionFromBundle :: NftBundle -> Maybe Auction.Auction
getAuctionFromBundle nftBundle = join (getAuctionFromLot <$> (nftBundle ^. _nbTokens ^? _HasLot . _2))

getSaleFromBundle :: NftBundle -> Maybe Sale.Sale
getSaleFromBundle nftBundle = join (getSaleFromLot <$> (nftBundle ^. _nbTokens ^? _HasLot . _2))

-- Calculates a hash of a list of ByteStrings,
-- the result does not depend on the order of ByteStrings inside a list
calcBundleIdHash :: [IpfsCid] -> BundleId
calcBundleIdHash = BA.convert . Hash.hashUpdates alg . HL.sort
  where
    alg = Hash.hashInit @Hash.SHA256

{-# INLINABLE makeBundle #-}
makeBundle :: AssocMap.Map IpfsCidHash NFT -> [IpfsCidHash] -> BundleInfo -> NftBundle
makeBundle singletons nftIds bundleInfo =
  NftBundle
    { nbRecord        = bundleInfo
    , nbTokens      = NoLot $ foldr insert AssocMap.empty nftIds
    }
  where
    insert nftId store = case AssocMap.lookup nftId singletons of
                                Just n -> AssocMap.insert nftId (nftRecord n) store
                                Nothing -> store

{-# INLINABLE bundleValue #-}
bundleValue :: AssocMap.Map IpfsCidHash IpfsCid -> NftBundle -> Value
bundleValue cids bundle = case nbTokens bundle of
      NoLot tokens    -> foldMap getValueNoLot $ AssocMap.toList tokens
      HasLot tokens _ -> foldMap getValueHasLot tokens
    where
      getValueHasLot :: (IpfsCid, NftInfo) -> Value
      getValueHasLot (ipfsCid, nft) = V.singleton (niCurrency nft) (V.TokenName ipfsCid) 1

      getValueNoLot :: (IpfsCidHash, NftInfo) -> Value
      getValueNoLot (ipfsCidHash, nft) = case AssocMap.lookup ipfsCidHash cids of
                                Just ipfsCid -> V.singleton (niCurrency nft) (V.TokenName ipfsCid) 1
                                Nothing -> mempty

{-# INLINABLE hasLotBundle #-}
hasLotBundle :: NftBundle -> Bool
hasLotBundle bundle = case nbTokens bundle of
  HasLot _ _ -> True
  _          -> False

{-# INLINABLE nftValue #-}
nftValue :: IpfsCid -> NFT -> Value
nftValue ipfsCid nft = V.singleton (niCurrency $ nftRecord nft) (V.TokenName ipfsCid) 1
