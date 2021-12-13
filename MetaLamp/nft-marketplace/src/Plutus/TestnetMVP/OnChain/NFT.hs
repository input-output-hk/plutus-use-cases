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
module Plutus.TestnetMVP.OnChain.NFT where

import Plutus.Contracts.NftMarketplace.OnChain.Core.NFT (NftInfo(..), IpfsCid, IpfsCidHash, Category)
import           Control.Lens                                           (_2,
                                                                         (&),
                                                                         (.~),
                                                                         (?~),
                                                                         (^.),
                                                                         (^?))
import qualified Control.Lens                                           as Lens
import           Control.Monad                                          (join)
import qualified Crypto.Hash                                            as Hash
import qualified Data.Aeson                                             as J
import qualified Data.ByteArray                                         as BA
import qualified Data.List                                              as HL
import qualified Data.Text                                              as T
import qualified GHC.Generics                                           as Haskell
import           Ledger
import qualified Ledger.Constraints                                     as Constraints
import qualified Ledger.Typed.Scripts                                   as Scripts
import qualified Ledger.Value                                           as V
import           Plutus.Abstract.Percentage                             (Percentage)
import           Plutus.Contract
import           Plutus.Contract.StateMachine
import qualified Plutus.Contract.StateMachine                           as SM
import           Plutus.Contracts.NftMarketplace.OffChain.Serialization (PlutusBuiltinByteString (..))
import qualified PlutusTx
import qualified PlutusTx.AssocMap                                      as AssocMap
import           PlutusTx.Prelude                                       hiding
                                                                        (Semigroup (..))
import           Prelude                                                (Semigroup (..))
import qualified Prelude                                                as Haskell
import qualified Plutus.TestnetMVP.Services.Sale.Script as Sale

data LotLink =
  SaleLotLink Sale.Sale
  deriving stock (Haskell.Eq, Haskell.Show, Haskell.Generic)
  deriving anyclass (J.ToJSON, J.FromJSON)

PlutusTx.unstableMakeIsData ''LotLink

PlutusTx.makeLift ''LotLink

Lens.makePrisms ''LotLink

getLotValue :: LotLink -> V.Value
getLotValue (SaleLotLink sale)       = Sale.saleValue sale

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

getSaleFromNFT :: NFT -> Maybe Sale.Sale
getSaleFromNFT nft =  nft ^. _nftLot ^? traverse . _2 . _SaleLotLink

{-# INLINABLE nftValue #-}
nftValue :: IpfsCid -> NFT -> Value
nftValue ipfsCid nft = V.singleton (niCurrency $ nftRecord nft) (V.TokenName ipfsCid) 1