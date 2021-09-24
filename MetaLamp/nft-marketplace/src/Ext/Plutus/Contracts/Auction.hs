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

module Ext.Plutus.Contracts.Auction where

import           Control.Lens                                     (makeClassyPrisms)
import           Data.Aeson                                       (FromJSON,
                                                                   ToJSON)
import           Data.Monoid                                      (Last (..))
import           Data.Semigroup.Generic                           (GenericSemigroupMonoid (..))
import           GHC.Generics                                     (Generic)
import           Ledger                                           (Ada,
                                                                   PubKeyHash,
                                                                   Slot, Value)
import qualified Ledger
import qualified Ledger.Ada                                       as Ada
import qualified Ledger.Constraints                               as Constraints
import           Ledger.Constraints.TxConstraints                 (TxConstraints)
import qualified Ledger.Interval                                  as Interval
import qualified Ledger.Typed.Scripts                             as Scripts
import           Ledger.Typed.Tx                                  (TypedScriptTxOut (..))
import           Ledger.Value                                     (AssetClass)
import           Plutus.Contract
import           Plutus.Contract.StateMachine                     hiding
                                                                  (mkValidator,
                                                                   typedValidator)
import qualified Plutus.Contract.StateMachine                     as SM
import           Plutus.Contract.Util                             (loopM)
import qualified Plutus.Contracts.Currency                        as Currency
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core.NFT as Core
import           Plutus.Types.Percentage                          (Percentage (..))
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified PlutusTx.Ratio                                   as Ratio
import qualified Prelude                                          as Haskell

-- | Definition of an auction
data AuctionParams
    = AuctionParams
        { apOwner               :: PubKeyHash -- ^ Current owner of the asset. This is where the proceeds of the auction will be sent.
        , apAsset               :: Value -- ^ The asset itself. This value is going to be locked by the auction script output.
        , apEndTime             :: Ledger.POSIXTime -- ^ When the time window for bidding ends.
        , apInitialPrice        :: Value
        , apMarketplaceOperator :: PubKeyHash
        , apMarketplaceSaleFee  :: Percentage
        }
        deriving stock (Haskell.Eq, Haskell.Show, Generic)
        deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''AuctionParams

PlutusTx.unstableMakeIsData ''AuctionParams

{-# INLINABLE fromAuction #-}
fromAuction :: Core.Auction -> AuctionParams
fromAuction Core.Auction {..} = AuctionParams {
    apOwner = aOwner,
    apAsset = aAsset,
    apEndTime = Ledger.POSIXTime aEndTime,
    apInitialPrice = aInitialPrice,
    apMarketplaceOperator = aMarketplaceOperator,
    apMarketplaceSaleFee = aMarketplaceSaleFee
    }

{-# INLINABLE toAuction #-}
toAuction :: SM.ThreadToken -> AuctionParams -> Core.Auction
toAuction threadToken AuctionParams {..} =
    Core.Auction {
        aThreadToken = threadToken
        , aOwner = apOwner
        , aAsset = apAsset
        , aInitialPrice = apInitialPrice
        , aEndTime = Ledger.getPOSIXTime apEndTime
        , aMarketplaceOperator = apMarketplaceOperator
        , aMarketplaceSaleFee = apMarketplaceSaleFee
    }

{-# INLINABLE getStateToken #-}
getStateToken :: Core.Auction -> SM.ThreadToken
getStateToken auction = Core.aThreadToken auction

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
    deriving stock (Generic, Haskell.Show)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''AuctionInput

type AuctionMachine = StateMachine AuctionState AuctionInput

{-# INLINABLE auctionTransition #-}
-- | The transitions of the auction state machine.
auctionTransition :: AuctionParams -> State AuctionState -> AuctionInput -> Maybe (TxConstraints Void Void, State AuctionState)
auctionTransition AuctionParams{..} State{stateData=oldState} input =
    case (oldState, input) of

        (Ongoing HighestBid{highestBid, highestBidder}, Bid{newBid, newBidder}) | newBid > highestBid -> -- if the new bid is higher,
            let constraints =
                    Constraints.mustPayToPubKey highestBidder (Ada.toValue highestBid) -- we pay back the previous highest bid
                    <> Constraints.mustValidateIn (Interval.to $ apEndTime - 1) -- but only if we haven't gone past 'apEndTime'
                newState =
                    State
                        { stateData = Ongoing HighestBid{highestBid = newBid, highestBidder = newBidder}
                        , stateValue = apAsset <> Ada.toValue newBid -- and lock the new bid in the script output
                        }
            in Just (constraints, newState)

        (Ongoing h@HighestBid{highestBidder, highestBid}, Payout) ->
            let constraints =
                    Constraints.mustValidateIn (Interval.from apEndTime) -- When the auction has ended,
                    <> Constraints.mustPayToPubKey apOwner (Ada.lovelaceValueOf saleProfit) -- the owner receives the payment
                    <> Constraints.mustPayToPubKey highestBidder apAsset -- and the highest bidder the asset
                    <> Constraints.mustPayToPubKey apMarketplaceOperator (Ada.lovelaceValueOf operatorFee)
                newState = State { stateData = Finished h, stateValue = mempty }
                highestBidInLovelace = Ada.getLovelace highestBid
                saleProfit = highestBidInLovelace - operatorFee
                operatorFee = Ratio.round $ (highestBidInLovelace % 100) * (getPercentage apMarketplaceSaleFee)
            in Just (constraints, newState)

        -- Any other combination of 'AuctionState' and 'AuctionInput' is disallowed.
        -- This rules out new bids that don't go over the current highest bid.
        _ -> Nothing


{-# INLINABLE auctionStateMachine #-}
auctionStateMachine :: (ThreadToken, AuctionParams) -> AuctionMachine
auctionStateMachine (threadToken, auctionParams) = SM.mkStateMachine (Just threadToken) (auctionTransition auctionParams) isFinal where
    isFinal Finished{} = True
    isFinal _          = False

{-# INLINABLE mkValidator #-}
mkValidator :: (ThreadToken, AuctionParams) -> Scripts.ValidatorType AuctionMachine
mkValidator = SM.mkValidator . auctionStateMachine

-- | The script instance of the auction state machine. It contains the state
--   machine compiled to a Plutus core validator script.
typedValidator :: (ThreadToken, AuctionParams) -> Scripts.TypedValidator AuctionMachine
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
    -> ThreadToken -- ^ Thread token of the instance
    -> AuctionParams
    -> StateMachineClient AuctionState AuctionInput
machineClient inst threadToken auctionParams =
    let machine = auctionStateMachine (threadToken, auctionParams)
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
    | AuctionContractError ContractError -- ^ Endpoint, coin selection, etc. failed
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''AuctionError

instance AsContractError AuctionError where
    _ContractError = _AuctionContractError . _ContractError

instance SM.AsSMContractError AuctionError where
    _SMContractError = _StateMachineContractError . SM._SMContractError

-- | Client code for the seller
startAuction :: AuctionParams -> Contract w s AuctionError SM.ThreadToken
startAuction auctionParams@AuctionParams{..} = do
    threadToken <- SM.getThreadToken
    logInfo $ "Obtained thread token: " <> Haskell.show threadToken
    let inst         = typedValidator (threadToken, auctionParams)
        client       = machineClient inst threadToken auctionParams

    _ <- handleError
            (\e -> do { logError (AuctionFailed e); throwError (StateMachineContractError e) })
            (SM.runInitialise client (initialState apOwner) apAsset)

    logInfo $ AuctionStarted auctionParams
    pure threadToken

-- | Client code for the seller
payoutAuction :: SM.ThreadToken -> AuctionParams -> Contract w s AuctionError ()
payoutAuction threadToken params = do
    let inst         = typedValidator (threadToken, params)
        client       = machineClient inst threadToken params

    _ <- awaitTime $ apEndTime params

    r <- SM.runStep client Payout
    case r of
        SM.TransitionFailure i            -> logError (TransitionFailed i)
        SM.TransitionSuccess (Finished h) -> logInfo $ AuctionEnded h
        SM.TransitionSuccess s            -> logWarn ("Unexpected state after Payout transition: " <> Haskell.show s)

-- | Get the current state of the contract and log it.
currentState :: SM.ThreadToken -> AuctionParams -> Contract w s AuctionError (Maybe AuctionState)
currentState threadToken params = mapError StateMachineContractError (SM.getOnChainState client) >>= \case
    Just (SM.OnChainState{SM.ocsTxOut=TypedScriptTxOut{tyTxOutData= s}}, _) -> do
        pure (Just s)
    _ -> do
        logWarn CurrentStateNotFound
        pure Nothing
    where
      inst         = typedValidator (threadToken, params)
      client       = machineClient inst threadToken params

submitBid :: SM.ThreadToken -> AuctionParams -> Ada -> Contract w s AuctionError ()
submitBid threadToken params ada = do
    let inst         = typedValidator (threadToken, params)
        client       = machineClient inst threadToken params
    self <- Ledger.pubKeyHash <$> ownPubKey
    let bid = Bid{newBid = ada, newBidder = self}
    _ <- SM.runStep client bid
    logInfo @Haskell.String $ "Bid submitted" <> Haskell.show bid
