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

module Contracts.MutualBet.Types
  where

import           Contracts.Oracle.Types
import           Data.Monoid                      (Last (..))
import           Data.Semigroup.Generic           (GenericSemigroupMonoid (..))
import           Ledger
import           Ledger.Oracle
import           Ledger.Value     
import           Playground.Contract (Show, FromJSON, Generic, ToJSON, ToSchema)
import           Plutus.Contract.StateMachine (ThreadToken)
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude              as Haskell



-- | Definition of an mutual bet
data MutualBetParams
    = MutualBetParams
        { mbpGame   :: Integer -- ^ Game id
        , mbpOracle :: Oracle
        , mbpTeam1 :: Integer
        , mbpTeam2 :: Integer
        }
        deriving stock (Haskell.Eq, Haskell.Show, Generic)
        deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''MutualBetParams

data NewBetParams = 
    NewBetParams
        { nbpAmount  :: Ada
        , nbpOutcome :: Integer
        }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

data Bet =
    Bet
        { amount  :: Ada
        , bettor  :: PubKeyHash
        , outcome :: Integer
        }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''Bet

-- | The states of the auction
data MutualBetState
    = Ongoing [Bet] -- Bids can be submitted.
    | Finished [Bet] -- The auction is finished
    deriving stock (Generic, Haskell.Show, Haskell.Eq)
    deriving anyclass (ToJSON, FromJSON)

-- | Observable state of the mutual bet app
data MutualBetOutput =
    MutualBetOutput
        { mutualBetState       :: Last MutualBetState
        , mutualBetThreadToken :: Last ThreadToken
        }
        deriving stock (Generic, Haskell.Show, Haskell.Eq)
        deriving anyclass (ToJSON, FromJSON)

deriving via (GenericSemigroupMonoid MutualBetOutput) instance (Haskell.Semigroup MutualBetOutput)
deriving via (GenericSemigroupMonoid MutualBetOutput) instance (Haskell.Monoid MutualBetOutput)

mutualBetStateOut :: MutualBetState -> MutualBetOutput
mutualBetStateOut s = Haskell.mempty { mutualBetState = Last (Just s) }

threadTokenOut :: ThreadToken -> MutualBetOutput
threadTokenOut t = Haskell.mempty { mutualBetThreadToken = Last (Just t) }

-- | Initial 'MutualBetState'. In the beginning there are no bets
initialState :: PubKeyHash -> MutualBetState
initialState self = Ongoing []

PlutusTx.unstableMakeIsData ''MutualBetState

-- | Transition between auction states
data MutualBetInput
    = NewBet { newBet :: Ada, newBettor :: PubKeyHash, newOutcome :: Integer } -- Increase the price
    | Payout { oracleValue :: OracleData }
    deriving stock (Generic, Haskell.Show)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''MutualBetInput