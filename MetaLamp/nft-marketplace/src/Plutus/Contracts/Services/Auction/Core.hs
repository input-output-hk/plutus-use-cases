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
import qualified Data.Text                    as T
import qualified GHC.Generics                 as Haskell
import           Ledger
import qualified Ledger.Ada                   as Ada
import qualified Ledger.Constraints           as Constraints
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Value
import           Plutus.Contract
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import qualified PlutusTx.AssocMap            as AssocMap
import           PlutusTx.Prelude             hiding (Semigroup (..))
import           Prelude                      (Semigroup (..))
import qualified Prelude                      as Haskell
import qualified Schema
import qualified Plutus.Abstract.Percentage as Percentage
import           Data.Aeson                                       (FromJSON,
                                                                   ToJSON)
import           GHC.Generics                                     (Generic)
import qualified Schema

data AuctionFee = 
    AuctionFee 
    { afAuctionOperator :: !PubKeyHash
    , afAuctionFee  :: !Percentage.Percentage
    }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, Schema.ToSchema)

PlutusTx.unstableMakeIsData ''AuctionFee

PlutusTx.makeLift ''AuctionFee

Lens.makeClassy_ ''AuctionFee

-- TODO can't use POSIXTime directly because of custom JSON instances defined in Plutus:
-- generated purescript type has generic instances
type POSIXTimeT = Integer

data AuctionForClient =
    AuctionForClient {
        afcProtocolToken :: !PubKeyHash,
        afcOwner :: !PubKeyHash,
        afcAsset :: !Value,
        afcEndTime :: !POSIXTimeT,   -- TODO: use Auction time for frontend after update plutus
        afcAuctionFee  :: Maybe AuctionFee
    }
        deriving stock (Haskell.Eq, Haskell.Show, Generic)
        deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''AuctionForClient

PlutusTx.unstableMakeIsData ''AuctionForClient

Lens.makeClassy_ ''AuctionForClient

-- | Definition of an auction
data Auction
    = Auction
        { aProtocolToken   :: !ThreadToken
        , aOwner               :: !PubKeyHash -- ^ Current owner of the asset. This is where the proceeds of the auction will be sent.
        , aAsset               :: !Value -- ^ The asset itself. This value is going to be locked by the auction script output.
        , aEndTime             :: !Ledger.POSIXTime -- ^ When the time window for bidding ends.
        , aAuctionFee       :: Maybe AuctionFee
        }
        deriving stock (Haskell.Eq, Haskell.Show, Generic)
        deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''Auction

PlutusTx.unstableMakeIsData ''Auction

Lens.makeClassy_ ''Auction

{-# INLINABLE getAuctionStateToken #-}
getAuctionStateToken :: Auction -> ThreadToken
getAuctionStateToken = aProtocolToken
