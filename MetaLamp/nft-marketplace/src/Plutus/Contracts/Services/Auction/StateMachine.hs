{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
module Plutus.Contracts.Services.Auction.StateMachine where
import           Control.Lens                           (makeClassyPrisms)
import           Data.Aeson                             (FromJSON, ToJSON)
import qualified Data.Aeson                             as J
import           Data.Monoid                            (Last (..))
import           Data.Semigroup.Generic                 (GenericSemigroupMonoid (..))
import           Ext.Plutus.Ledger.Index                (minAdaTxOutValue)
import           GHC.Generics                           (Generic)
import           Ledger                                 (Ada, PubKeyHash, Slot,
                                                         Value)
import qualified Ledger
import qualified Ledger.Ada                             as Ada
import qualified Ledger.Constraints                     as Constraints
import           Ledger.Constraints.TxConstraints       (TxConstraints)
import qualified Ledger.Interval                        as Interval
import qualified Ledger.Typed.Scripts                   as Scripts
import           Ledger.Typed.Tx                        (TypedScriptTxOut (..))
import           Ledger.Value                           (AssetClass)
import qualified Plutus.Abstract.Percentage             as Percentage
import qualified Plutus.Abstract.PercentageInterface    as Percentage
import           Plutus.Contract
import           Plutus.Contract.StateMachine           hiding (mkValidator,
                                                         typedValidator)
import qualified Plutus.Contract.StateMachine           as SM
import           Plutus.Contract.Util                   (loopM)
import           Plutus.Contracts.Services.Auction.Core
import           Plutus.V1.Ledger.Time                  (POSIXTime (..))
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude                                as Haskell
import qualified Schema

data HighestBid =
    HighestBid
        { highestBid    :: Ada
        , highestBidder :: PubKeyHash
        }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''HighestBid

-- | The states of the auction
data AuctionState
    = Ongoing HighestBid -- Bids can be submitted.
    | Finished HighestBid -- The auction is finished
    | Canceled
    deriving stock (Generic, Haskell.Show, Haskell.Eq)
    deriving anyclass (ToJSON, FromJSON)

-- | Observable state of the auction app
data AuctionOutput =
    AuctionOutput
        { auctionState       :: Last AuctionState
        , auctionThreadToken :: Last SM.ThreadToken
        }
        deriving stock (Generic, Haskell.Show, Haskell.Eq)
        deriving anyclass (ToJSON, FromJSON)

deriving via (GenericSemigroupMonoid AuctionOutput) instance (Haskell.Semigroup AuctionOutput)
deriving via (GenericSemigroupMonoid AuctionOutput) instance (Haskell.Monoid AuctionOutput)

auctionStateOut :: AuctionState -> AuctionOutput
auctionStateOut s = Haskell.mempty { auctionState = Last (Just s) }

threadTokenOut :: SM.ThreadToken -> AuctionOutput
threadTokenOut t = Haskell.mempty { auctionThreadToken = Last (Just t) }

-- | Initial 'AuctionState'. In the beginning the highest bid is 0 and the
--   highest bidder is seller of the asset. So if nobody submits
--   any bids, the seller gets the asset back after the auction has ended.
initialState :: PubKeyHash -> AuctionState
initialState self = Ongoing HighestBid{highestBid = 0, highestBidder = self}

PlutusTx.unstableMakeIsData ''AuctionState

-- | Transition between auction states
data AuctionInput
    = Bid { newBid :: Ada, newBidder :: PubKeyHash } -- Increase the price
    | Payout
    | Cancel
    deriving stock (Generic, Haskell.Show)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''AuctionInput

type AuctionMachine = StateMachine AuctionState AuctionInput

type GetAdditionalConstraints = Auction -> State AuctionState -> TxConstraints Void Void

auctionWithFeePayoutConstraints :: AuctionFee -> GetAdditionalConstraints
auctionWithFeePayoutConstraints AuctionFee{..} Auction{..} State {stateData = (Ongoing HighestBid{highestBidder, highestBid})} =
    let highestBidInLovelace = Ada.getLovelace highestBid
        saleProfit = highestBidInLovelace - fee
        fee = Percentage.calculatePercentageRounded afAuctionFee highestBidInLovelace
    in
        Constraints.mustPayToPubKey aOwner (Ada.lovelaceValueOf saleProfit) <>
        Constraints.mustPayToPubKey afAuctionOperator (Ada.lovelaceValueOf fee)
auctionWithFeePayoutConstraints _ _ _ = mempty

auctionWithoutFeePayoutConstraints :: GetAdditionalConstraints
auctionWithoutFeePayoutConstraints Auction{..} State {stateData = (Ongoing HighestBid{highestBidder, highestBid})} =
        Constraints.mustPayToPubKey aOwner (Ada.toValue highestBid)
auctionWithoutFeePayoutConstraints _ _ = mempty


{-# INLINABLE auctionTransition #-}
-- | The transitions of the auction state machine.
auctionTransition :: GetAdditionalConstraints -> Auction -> State AuctionState -> AuctionInput -> Maybe (TxConstraints Void Void, State AuctionState)
auctionTransition getAdditionalPayoutConstraints params@Auction{..} state@State{stateData=oldState} input =
    case (oldState, input) of

        (Ongoing HighestBid{highestBid, highestBidder}, Bid{newBid, newBidder}) | (newBid >= aInitialPrice) && (newBid > highestBid) -> -- if the new bid is higher,
            let constraints =
                    Constraints.mustPayToPubKey highestBidder (Ada.toValue highestBid) -- we pay back the previous highest bid
                    <> Constraints.mustValidateIn (Interval.to $ aEndTime - 1) -- but only if we haven't gone past 'aEndTime'
                newState =
                    State
                        { stateData = Ongoing HighestBid{highestBid = newBid, highestBidder = newBidder}
                        , stateValue = aAsset <> Ada.toValue newBid -- and lock the new bid in the script output
                        }
            in Just (constraints, newState)

        (Ongoing h@HighestBid{highestBidder, highestBid}, Payout) ->
            let
                additionalConstraints = getAdditionalPayoutConstraints params state
                constraints =
                    Constraints.mustPayToPubKey highestBidder (aAsset + minAdaTxOutValue) -- and the highest bidder the asset
                    <> additionalConstraints
                    <> Constraints.mustValidateIn (Interval.from aEndTime) -- When the auction has ended,
                newState = State { stateData = Finished h, stateValue = mempty }
            in Just (constraints, newState)

        (Ongoing h@HighestBid{highestBidder, highestBid}, Cancel) ->
            let
                constraints =
                    Constraints.mustValidateIn (Interval.to aEndTime) -- While the auction hasn't ended,
                    <> Constraints.mustPayToPubKey highestBidder (Ada.toValue highestBid) -- and the highest bidder the asset
                    <> Constraints.mustPayToPubKey aOwner (aAsset + minAdaTxOutValue) -- and the highest bidder the asset
                    -- TODO: is it okay that buyer receive additional 2ADA? Should we initially add them to the bid price?
                newState = State { stateData = Canceled, stateValue = mempty }
            in Just (constraints, newState)

        -- Any other combination of 'AuctionState' and 'AuctionInput' is disallowed.
        -- This rules out new bids that don't go over the current highest bid.
        _ -> Nothing

{-# INLINABLE auctionStateMachine #-}
auctionStateMachine :: Auction -> AuctionMachine
auctionStateMachine auction =
    SM.mkStateMachine (Just $ aProtocolToken auction) (transition $ aAuctionFee auction) isFinal where
    isFinal Finished{} = True
    isFinal _          = False
    transition (Just auctionFee) = auctionTransition (auctionWithFeePayoutConstraints auctionFee) auction
    transition Nothing = auctionTransition auctionWithoutFeePayoutConstraints auction

{-# INLINABLE mkValidator #-}
mkValidator :: Auction -> Scripts.ValidatorType AuctionMachine
mkValidator = SM.mkValidator . auctionStateMachine

-- | The script instance of the auction state machine. It contains the state
--   machine compiled to a Plutus core validator script.
typedValidator :: Auction -> Scripts.TypedValidator AuctionMachine
typedValidator = Scripts.mkTypedValidatorParam @AuctionMachine
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator

-- | The machine client of the auction state machine. It contains the script instance
--   with the on-chain code, and the Haskell definition of the state machine for
--   off-chain use.
machineClient
    :: Scripts.TypedValidator AuctionMachine
    -> Auction
    -> StateMachineClient AuctionState AuctionInput
machineClient inst auction =
    let machine = auctionStateMachine auction
    in SM.mkStateMachineClient (SM.StateMachineInstance machine inst)
