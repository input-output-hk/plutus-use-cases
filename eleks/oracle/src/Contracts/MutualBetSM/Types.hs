{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# options_ghc -fno-warn-orphans          #-}
{-# options_ghc -Wno-redundant-constraints #-}
{-# options_ghc -fno-strictness            #-}
{-# options_ghc -fno-specialise            #-}

module Contracts.MutualBetSM.Types
  where

import           Contracts.Oracle.Types
import           Data.Monoid                      (Last (..))
import           Data.Semigroup.Generic           (GenericSemigroupMonoid (..))
import           Ledger
import           Plutus.Contract.Oracle 
import           Ledger.Value     
import           Playground.Contract              (Show, FromJSON, Generic, ToJSON, ToSchema)
import qualified Plutus.Contract.StateMachine     as SM
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude                          as Haskell
import           Types.Game
import qualified Data.OpenApi.Schema              as OpenApi

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

PlutusTx.unstableMakeIsData ''Bet

instance Eq Bet where
    {-# INLINABLE (==) #-}
    l == r = (betAmount l == betAmount r) && 
             (betBettor l == betBettor r) &&
             (betTeamId l == betTeamId r)

-- | The states of the auction
data MutualBetState
    = Ongoing  [Bet] -- Bids can be submitted.
    | BettingClosed  [Bet] -- Bids can not be submitted
    | Finished [Bet] -- The auction is finished
    deriving stock (Generic, Haskell.Show, Haskell.Eq)
    deriving anyclass (ToJSON, FromJSON)

-- | Observable state of the mutual bet app
data MutualBetOutput =
    MutualBetOutput
        { mutualBetState       :: Last MutualBetState
        , mutualBetThreadToken :: Last SM.ThreadToken
        }
        deriving stock (Generic, Haskell.Show, Haskell.Eq)
        deriving anyclass (ToJSON, FromJSON)

deriving via (GenericSemigroupMonoid MutualBetOutput) instance (Haskell.Semigroup MutualBetOutput)
deriving via (GenericSemigroupMonoid MutualBetOutput) instance (Haskell.Monoid MutualBetOutput)

mutualBetStateOut :: MutualBetState -> MutualBetOutput
mutualBetStateOut s = Haskell.mempty { mutualBetState = Last (Just s) }

threadTokenOut :: SM.ThreadToken -> MutualBetOutput
threadTokenOut t = Haskell.mempty { mutualBetThreadToken = Last (Just t) }

-- | Initial 'MutualBetState'. In the beginning there are no bets
initialState :: PubKeyHash -> MutualBetState
initialState self = Ongoing []

PlutusTx.unstableMakeIsData ''MutualBetState

-- | Transition between auction states
data MutualBetInput
    = NewBet { newBet :: Bet } -- Increase the price
    | CancelBet { cancelBet :: Bet }
    | FinishBetting { oracleSigned :: SignedMessage OracleSignedMessage }
    | Payout { oracleValue :: OracleData, oracleRef :: TxOutRef, oracleSigned :: SignedMessage OracleSignedMessage }
    | CancelGame
    deriving stock (Generic, Haskell.Show)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''MutualBetInput

data MutualBetLog =
    MutualBetStarted MutualBetParams -- Contract started
    | MutualBetFailed SM.SMContractError -- Contract start erro
    | BetSubmitted [Bet] -- bet submitted
    | BetCancelled [Bet]
    | MutualBetBettingClosed [Bet] -- Betting not allowed
    | MutualBetCancelled [Bet] -- Game cancelled
    | MutualBetGameEnded [Bet] -- Game completed
    | CurrentStateNotFound -- Contract state not found
    | TransitionFailed (SM.InvalidTransition MutualBetState MutualBetInput)
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)