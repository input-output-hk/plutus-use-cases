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
{-# LANGUAGE StandaloneDeriving    #-}

module Plutus.Contracts.Services.Auction.Core where

import qualified Control.Lens                 as Lens
import           Data.Aeson                   (FromJSON, ToJSON)
import qualified Data.Text                    as T
import           GHC.Generics                 (Generic)
import qualified GHC.Generics                 as Haskell
import           Ledger
import qualified Ledger.Ada                   as Ada
import qualified Ledger.Constraints           as Constraints
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Value
import qualified Plutus.Abstract.Percentage   as Percentage
import           Plutus.Contract
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import qualified PlutusTx.AssocMap            as AssocMap
import           PlutusTx.Prelude             hiding (Semigroup (..))
import           Prelude                      (Semigroup (..))
import qualified Prelude                      as Haskell
import qualified Schema

data AuctionFee =
    AuctionFee
    { afAuctionOperator :: !PubKeyHash
    , afAuctionFee      :: !Percentage.Percentage
    }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, Schema.ToSchema)

PlutusTx.unstableMakeIsData ''AuctionFee

PlutusTx.makeLift ''AuctionFee

Lens.makeClassy_ ''AuctionFee

-- | Definition of an auction
data Auction
    = Auction
        { aProtocolToken :: !ThreadToken
        , aOwner         :: !PubKeyHash
        , aAsset         :: !Value
        , aInitialPrice  :: !Ada
        , aEndTime       :: !Ledger.POSIXTime
        , aAuctionFee    :: Maybe AuctionFee
        }
        deriving stock (Haskell.Eq, Haskell.Show, Generic)
        deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''Auction

PlutusTx.unstableMakeIsData ''Auction

Lens.makeClassy_ ''Auction

{-# INLINABLE getAuctionStateToken #-}
getAuctionStateToken :: Auction -> ThreadToken
getAuctionStateToken = aProtocolToken
