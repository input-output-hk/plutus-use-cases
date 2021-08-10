{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fobject-code #-}

module Plutus.Contracts.NftMarketplace.OnChain.Core.NFT where

import           Control.Lens                   ((&), (.~), (?~), (^.))
import qualified Control.Lens                   as Lens
import qualified Crypto.Hash                    as Hash
import qualified Data.Aeson                     as J
import qualified Data.ByteArray                 as BA
import qualified Data.List                      as HL
import qualified Data.Text                      as T
import qualified Ext.Plutus.Contracts.Auction   as Auction
import qualified GHC.Generics                   as Haskell
import           Ledger
import qualified Ledger.Constraints             as Constraints
import qualified Ledger.Typed.Scripts           as Scripts
import qualified Ledger.Value                   as V
import           Plutus.Contract
import           Plutus.Contract.StateMachine
import qualified Plutus.Contracts.Services.Sale as Sale
import qualified PlutusTx
import qualified PlutusTx.AssocMap              as AssocMap
import           PlutusTx.Prelude               hiding (Semigroup (..))
import           Prelude                        (Semigroup (..))
import qualified Prelude                        as Haskell

-- TODO (?) add tags
type IpfsCid = ByteString
type IpfsCidHash = ByteString
type Auction = (AssetClass, PubKeyHash, Value, Slot)
type Category = [ByteString]
type LotLink = Either Sale.Sale Auction
type BundleId = ByteString

data NftInfo =
  NftInfo
    { niCurrency    :: !CurrencySymbol
    , niName        :: !ByteString
    , niDescription :: !ByteString
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
    { biName        :: !ByteString
    , biDescription :: !ByteString
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

{-# INLINABLE hasLotNft #-}
hasLotNft :: NFT -> Bool
hasLotNft = isJust . nftLot

{-# INLINABLE hasLotBundle #-}
hasLotBundle :: NftBundle -> Bool
hasLotBundle bundle = case nbTokens bundle of
  HasLot _ _ -> True
  _          -> False
