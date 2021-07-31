{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
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

module Ext.Plutus.Contracts.Auction where

import           Control.Lens                     (makeClassyPrisms)
import           Data.Aeson                       (FromJSON, ToJSON)
import           Data.Monoid                      (Last (..))
import           Data.Semigroup.Generic           (GenericSemigroupMonoid (..))
import           GHC.Generics                     (Generic)
import           Ledger                           (Ada, PubKeyHash, Slot, Value)
import qualified Ledger
import qualified Ledger.Ada                       as Ada
import qualified Ledger.Constraints               as Constraints
import           Ledger.Constraints.TxConstraints (TxConstraints)
import qualified Ledger.Interval                  as Interval
import qualified Ledger.Typed.Scripts             as Scripts
import           Ledger.Typed.Tx                  (TypedScriptTxOut (..))
import           Ledger.Value                     (AssetClass)
import           Plutus.Contract
import           Plutus.Contract.StateMachine     (State (..),
                                                   StateMachine (..),
                                                   StateMachineClient, Void,
                                                   WaitingResult (..))
import qualified Plutus.Contract.StateMachine     as SM
import           Plutus.Contract.Util             (loopM)
import qualified Plutus.Contracts.Currency        as Currency
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude                          as Haskell

-- | Definition of an auction
data AuctionParams
    = AuctionParams
        { apOwner   :: PubKeyHash -- ^ Current owner of the asset. This is where the proceeds of the auction will be sent.
        , apAsset   :: Value -- ^ The asset itself. This value is going to be locked by the auction script output.
        , apEndTime :: Slot -- ^ When the time window for bidding ends.
        }
        deriving stock (Haskell.Eq, Haskell.Show, Generic)
        deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''AuctionParams
PlutusTx.unstableMakeIsData ''AuctionParams

{-# INLINABLE fromTuple #-}
fromTuple :: (AssetClass, PubKeyHash, Value, Slot) -> AuctionParams
fromTuple (_, apOwner, apAsset, apEndTime) = AuctionParams {..}

{-# INLINABLE toTuple #-}
toTuple :: AssetClass -> AuctionParams -> (AssetClass, PubKeyHash, Value, Slot)
toTuple threadToken AuctionParams {..} = (threadToken, apOwner, apAsset, apEndTime)

{-# INLINABLE getStateToken #-}
getStateToken :: (AssetClass, PubKeyHash, Value, Slot) -> AssetClass
getStateToken (token, _, _, _) = token

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
    deriving stock (Generic, Haskell.Show, Haskell.Eq)
    deriving anyclass (ToJSON, FromJSON)

-- | Observable state of the auction app
data AuctionOutput =
    AuctionOutput
        { auctionState       :: Last AuctionState
        , auctionThreadToken :: Last AssetClass
        }
        deriving stock (Generic, Haskell.Show, Haskell.Eq)
        deriving anyclass (ToJSON, FromJSON)

deriving via (GenericSemigroupMonoid AuctionOutput) instance (Haskell.Semigroup AuctionOutput)
deriving via (GenericSemigroupMonoid AuctionOutput) instance (Haskell.Monoid AuctionOutput)

auctionStateOut :: AuctionState -> AuctionOutput
auctionStateOut s = Haskell.mempty { auctionState = Last (Just s) }

threadTokenOut :: AssetClass -> AuctionOutput
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
    deriving stock (Generic, Haskell.Show)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''AuctionInput

{-# INLINABLE auctionTransition #-}
-- | The transitions of the auction state machine.
auctionTransition :: AuctionParams -> State AuctionState -> AuctionInput -> Maybe (TxConstraints Void Void, State AuctionState)

auctionTransition AuctionParams{apOwner, apAsset, apEndTime} State{stateData=oldState} input =
    case (oldState, input) of

        (Ongoing HighestBid{highestBid, highestBidder}, Bid{newBid, newBidder}) | newBid > highestBid -> -- if the new bid is higher,
            let constraints =
                    Constraints.mustPayToPubKey highestBidder (Ada.toValue highestBid) -- we pay back the previous highest bid
                    <> Constraints.mustValidateIn (Interval.to apEndTime) -- but only if we haven't gone past 'apEndTime'
                newState =
                    State
                        { stateData = Ongoing HighestBid{highestBid = newBid, highestBidder = newBidder}
                        , stateValue = apAsset <> Ada.toValue newBid -- and lock the new bid in the script output
                        }
            in Just (constraints, newState)

        (Ongoing h@HighestBid{highestBidder, highestBid}, Payout) ->
            let constraints =
                    Constraints.mustValidateIn (Interval.from (apEndTime + 1)) -- When the auction has ended,
                    <> Constraints.mustPayToPubKey apOwner (Ada.toValue highestBid) -- the owner receives the payment
                    <> Constraints.mustPayToPubKey highestBidder apAsset -- and the highest bidder the asset
                newState = State { stateData = Finished h, stateValue = mempty }
            in Just (constraints, newState)

        -- Any other combination of 'AuctionState' and 'AuctionInput' is disallowed.
        -- This rules out new bids that don't go over the current highest bid.
        _ -> Nothing


{-# INLINABLE auctionStateMachine #-}
auctionStateMachine :: AssetClass -> AuctionParams -> StateMachine AuctionState AuctionInput
auctionStateMachine threadToken auctionParams = SM.mkStateMachine (Just threadToken) (auctionTransition auctionParams) isFinal where
    isFinal Finished{} = True
    isFinal _          = False


-- | The script instance of the auction state machine. It contains the state
--   machine compiled to a Plutus core validator script.
typedValidator :: AssetClass -> AuctionParams -> Scripts.TypedValidator (StateMachine AuctionState AuctionInput)
typedValidator currency auctionParams =
    let val = $$(PlutusTx.compile [|| validatorParam ||])
            `PlutusTx.applyCode`
                PlutusTx.liftCode currency
                `PlutusTx.applyCode`
                    PlutusTx.liftCode auctionParams
        validatorParam c f = SM.mkValidator (auctionStateMachine c f)
        wrap = Scripts.wrapValidator @AuctionState @AuctionInput

    in Scripts.mkTypedValidator @(StateMachine AuctionState AuctionInput)
        val
        $$(PlutusTx.compile [|| wrap ||])

-- | The machine client of the auction state machine. It contains the script instance
--   with the on-chain code, and the Haskell definition of the state machine for
--   off-chain use.
machineClient
    :: Scripts.TypedValidator (StateMachine AuctionState AuctionInput)
    -> AssetClass -- ^ Thread token of the instance
    -> AuctionParams
    -> StateMachineClient AuctionState AuctionInput
machineClient inst threadToken auctionParams =
    let machine = auctionStateMachine threadToken auctionParams
    in SM.mkStateMachineClient (SM.StateMachineInstance machine inst)

type BuyerSchema = Endpoint "bid" Ada
type SellerSchema = EmptySchema -- Don't need any endpoints: the contract runs automatically until the auction is finished.

data AuctionLog =
    AuctionStarted AuctionParams
    | AuctionFailed SM.SMContractError
    | BidSubmitted HighestBid
    | AuctionEnded HighestBid
    | CurrentStateNotFound
    | TransitionFailed (SM.InvalidTransition AuctionState AuctionInput)
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data AuctionError =
    StateMachineContractError SM.SMContractError -- ^ State machine operation failed
    | ThreadTokenError Currency.CurrencyError -- ^ Thread token could not be created
    | AuctionContractError ContractError -- ^ Endpoint, coin selection, etc. failed
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''AuctionError

instance AsContractError AuctionError where
    _ContractError = _AuctionContractError . _ContractError

instance SM.AsSMContractError AuctionError where
    _SMContractError = _StateMachineContractError . SM._SMContractError


-- | Client code for the seller
startAuction :: Value -> Slot -> Contract w s AuctionError (AssetClass, AuctionParams)
startAuction value slot = do
    threadToken <- mapError ThreadTokenError Currency.createThreadToken
    logInfo $ "Obtained thread token: " <> Haskell.show threadToken
    self <- Ledger.pubKeyHash <$> ownPubKey
    let params       = AuctionParams{apOwner = self, apAsset = value, apEndTime = slot }
        inst         = typedValidator threadToken params
        client       = machineClient inst threadToken params

    _ <- handleError
            (\e -> do { logError (AuctionFailed e); throwError (StateMachineContractError e) })
            (SM.runInitialise client (initialState self) value)

    logInfo $ AuctionStarted params
    pure (threadToken, params)

-- | Client code for the seller
payoutAuction :: AssetClass -> AuctionParams -> Contract w s AuctionError ()
payoutAuction threadToken params = do
    let inst         = typedValidator threadToken params
        client       = machineClient inst threadToken params

    _ <- awaitSlot $ apEndTime params

    r <- SM.runStep client Payout
    case r of
        SM.TransitionFailure i            -> logError (TransitionFailed i)
        SM.TransitionSuccess (Finished h) -> logInfo $ AuctionEnded h
        SM.TransitionSuccess s            -> logWarn ("Unexpected state after Payout transition: " <> Haskell.show s)

-- | Get the current state of the contract and log it.
currentState :: AssetClass -> AuctionParams -> Contract w s AuctionError (Maybe AuctionState)
currentState threadToken params = mapError StateMachineContractError (SM.getOnChainState client) >>= \case
    Just ((TypedScriptTxOut{tyTxOutData = s}, _), _) -> do
        pure (Just s)
    _ -> do
        logWarn CurrentStateNotFound
        pure Nothing
    where
      inst         = typedValidator threadToken params
      client       = machineClient inst threadToken params

submitBid :: AssetClass -> AuctionParams -> Ada -> Contract w s AuctionError ()
submitBid threadToken params ada = do
    let inst         = typedValidator threadToken params
        client       = machineClient inst threadToken params
    self <- Ledger.pubKeyHash <$> ownPubKey
    let bid = Bid{newBid = ada, newBidder = self}
    _ <- SM.runStep client bid
    logInfo @Haskell.String $ "Bid submitted" <> Haskell.show bid
