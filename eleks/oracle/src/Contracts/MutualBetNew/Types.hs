{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# options_ghc -fno-warn-orphans          #-}
{-# options_ghc -Wno-redundant-constraints #-}
{-# options_ghc -fno-strictness            #-}
{-# options_ghc -fno-specialise            #-}

module Contracts.MutualBetNew.Types
  where

import           Control.Monad                   (mzero)
import           Data.Aeson
import           Data.Aeson.TH   
import           Data.Aeson.Types
import           Data.Either                     (fromRight)
import           Data.Map                        (lookup)
import           Ledger                          hiding (txOutRefs)
import           Ledger.Crypto                   (pubKeyHash)
import           Playground.Contract             (Show, FromJSON, Generic, ToJSON, ToSchema)
import           Plutus.Contract.Oracle          (SignedMessage(..))
import           Plutus.ChainIndex.Tx            (txOutRefs, ChainIndexTx (..), ChainIndexTxOutputs (..))
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude                         as Haskell
import qualified Data.OpenApi.Schema             as OpenApi
import           Types.Game
import           Contracts.Oracle.Types

data MutualBetStartParams
    = MutualBetStartParams
        { mbspGame   :: Integer -- Game id
        , mbspOracle :: Oracle -- Oracle data
        , mbspOwner  :: PubKeyHash -- Owner of the platform , used for fee sending
        , mbspTeam1  :: Integer -- Team 1 id
        , mbspTeam2  :: Integer -- Team 2 id
        , mbspMinBet :: Ada -- Minimum bet allowed
        , mbspBetFee :: Ada -- Platform fee, for each bet you need additionally to pay the fee, fee is no returned if game in case game cancelled or no one wins
        }
        deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
        deriving anyclass (ToJSON, FromJSON, ToSchema, OpenApi.ToSchema)

-- | Definition of an mutual bet
data MutualBetParams
    = MutualBetParams
        { mbpGame   :: Integer -- Game id
        , mbpOracle :: Oracle -- Oracle data
        , mbpOwner  :: PubKeyHash -- Owner of the platform , used for fee sending
        , mbpTeam1  :: Integer -- Team 1 id
        , mbpTeam2  :: Integer -- Team 2 id
        , mbpMinBet :: Ada -- Minimum bet allowed
        , mbpBetFee :: Ada -- Platform fee, for each bet you need additionally to pay the fee, fee is no returned if game in case game cancelled or no one wins
        , mbpMutualBetId :: AssetClass
        }
        deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
        deriving anyclass (ToJSON, FromJSON, ToSchema, OpenApi.ToSchema)

PlutusTx.makeLift ''MutualBetParams

data Bet =
    Bet
        { betAmount   :: Ada
        , betBettor   :: PubKeyHash
        , betTeamId   :: Integer
        , betWinShare :: Ada
        }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''Bet [('Bet, 0)]
PlutusTx.makeLift ''Bet

instance Eq Bet where
    {-# INLINABLE (==) #-}
    l == r = (betAmount l == betAmount r) && 
             (betBettor l == betBettor r) &&
             (betTeamId l == betTeamId r)


data MutualBetRedeemer = MakeBet Bet | CancelBet Bet | StartGame (SignedMessage OracleSignedMessage) | Payout (SignedMessage OracleSignedMessage) | CancelGame (SignedMessage OracleSignedMessage)
    deriving Show
PlutusTx.makeIsDataIndexed ''MutualBetRedeemer [('MakeBet, 0), ('CancelBet, 1), ('StartGame, 2), ('Payout, 3), ('CancelGame, 4)]

type IsBetClosed = Bool
data MutualBetDatum =
      MutualBetDatum [Bet] IsBetClosed
    deriving stock (Show)

PlutusTx.makeIsDataIndexed ''MutualBetDatum [('MutualBetDatum, 0)]
PlutusTx.makeLift ''MutualBetDatum