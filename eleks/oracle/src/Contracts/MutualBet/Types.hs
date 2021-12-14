{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# options_ghc -fno-warn-orphans          #-}
{-# options_ghc -Wno-redundant-constraints #-}
{-# options_ghc -fno-strictness            #-}
{-# options_ghc -fno-specialise            #-}

module Contracts.MutualBet.Types
  where

import Contracts.Oracle.Types
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Either (fromRight)
import Data.Map (lookup)
import Data.Monoid (Last (..))
import Data.OpenApi.Schema qualified as OpenApi
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Ledger hiding (txOutRefs)
import Ledger.Crypto (pubKeyHash)
import Playground.Contract (FromJSON, Generic, Show, ToJSON, ToSchema)
import Plutus.ChainIndex.Tx (ChainIndexTx (..), ChainIndexTxOutputs (..), txOutRefs)
import Plutus.Contract.Oracle (SignedMessage (..))
import PlutusTx qualified
import PlutusTx.Prelude
import Prelude qualified as Haskell
import Types.Game

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
        { mbpGame        :: Integer -- Game id
        , mbpOracle      :: Oracle -- Oracle data
        , mbpOwner       :: PubKeyHash -- Owner of the platform , used for fee sending
        , mbpTeam1       :: Integer -- Team 1 id
        , mbpTeam2       :: Integer -- Team 2 id
        , mbpMinBet      :: Ada -- Minimum bet allowed
        , mbpBetFee      :: Ada -- Platform fee, for each bet you need additionally to pay the fee, fee is no returned if game in case game cancelled or no one wins
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


data MutualBetRedeemer =
    MakeBet Bet
    | CancelBet Bet
    | StartGame (SignedMessage OracleSignedMessage)
    | Payout (SignedMessage OracleSignedMessage)
    | CancelGame (SignedMessage OracleSignedMessage)
    | DeleteGame (SignedMessage OracleSignedMessage)
    deriving Show
PlutusTx.makeIsDataIndexed ''MutualBetRedeemer [('MakeBet, 0), ('CancelBet, 1), ('StartGame, 2), ('Payout, 3), ('CancelGame, 4), ('DeleteGame, 5)]

data GameStatus = BettingOpen | BettingClosed | GameCancelled | GameFinished
   deriving stock (Show)
PlutusTx.makeIsDataIndexed ''GameStatus [('BettingOpen, 0), ('BettingClosed, 1), ('GameCancelled, 2), ('GameFinished, 3)]
PlutusTx.makeLift ''GameStatus

type IsBetClosed = Bool
data MutualBetDatum =
      MutualBetDatum [Bet] GameStatus
    deriving stock (Show)

PlutusTx.makeIsDataIndexed ''MutualBetDatum [('MutualBetDatum, 0)]
PlutusTx.makeLift ''MutualBetDatum

data MutualBetState =
      BetState [Bet]
      | BettingClosedState [Bet]
      | CancelBetState [Bet]
      | CancelGameState [Bet]
      | Finished [Bet]
    deriving stock (Generic, Haskell.Show, Haskell.Eq)
    deriving anyclass (ToJSON, FromJSON)

-- | Observable state of the mutual bet app
data MutualBetOutput =
    MutualBetOutput
        { mutualBetState       :: Last MutualBetState
        }
        deriving stock (Generic, Haskell.Show, Haskell.Eq)
        deriving anyclass (ToJSON, FromJSON)

deriving via (GenericSemigroupMonoid MutualBetOutput) instance (Haskell.Semigroup MutualBetOutput)
deriving via (GenericSemigroupMonoid MutualBetOutput) instance (Haskell.Monoid MutualBetOutput)

mutualBetStateOut :: MutualBetState -> MutualBetOutput
mutualBetStateOut s = Haskell.mempty { mutualBetState = Last (Just s) }
