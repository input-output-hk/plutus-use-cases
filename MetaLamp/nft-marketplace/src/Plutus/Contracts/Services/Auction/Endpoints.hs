{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Plutus.Contracts.Services.Auction.Endpoints where
import           Control.Lens                                   (makeClassyPrisms)
import           Data.Aeson                                     (FromJSON,
                                                                 ToJSON)
import qualified Data.Aeson                                     as J
import           Data.Monoid                                    (Last (..))
import           Data.Semigroup.Generic                         (GenericSemigroupMonoid (..))
import qualified Data.Text                                      as T
import           GHC.Generics                                   (Generic)
import           Ledger                                         (Ada,
                                                                 PubKeyHash,
                                                                 Slot, Value)
import qualified Ledger
import qualified Ledger.Ada                                     as Ada
import qualified Ledger.Constraints                             as Constraints
import           Ledger.Constraints.TxConstraints               (TxConstraints)
import qualified Ledger.Interval                                as Interval
import qualified Ledger.Typed.Scripts                           as Scripts
import           Ledger.Typed.Tx                                (TypedScriptTxOut (..))
import           Ledger.Value                                   (AssetClass)
import qualified Plutus.Abstract.Percentage                     as Percentage
import qualified Plutus.Abstract.PercentageInterface            as Percentage
import           Plutus.Contract
import           Plutus.Contract.Request                        (ownPubKeyHash)
import           Plutus.Contract.StateMachine                   hiding
                                                                (mkValidator,
                                                                 typedValidator)
import qualified Plutus.Contract.StateMachine                   as SM
import           Plutus.Contract.Util                           (loopM)
import qualified Plutus.Contracts.Currency                      as Currency
import           Plutus.Contracts.Services.Auction.Core
import           Plutus.Contracts.Services.Auction.StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude                                        as Haskell
import qualified Schema

data StartAuctionParams = StartAuctionParams {
    sapOwner        :: !PubKeyHash,
    sapAsset        :: !Value,
    sapInitialPrice :: !Ada,
    sapEndTime      :: !Ledger.POSIXTime,
    sapAuctionFee   :: Maybe AuctionFee
}
    deriving stock    (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

PlutusTx.unstableMakeIsData ''StartAuctionParams
PlutusTx.makeLift ''StartAuctionParams

-- | Client code for the seller
startAuction :: StartAuctionParams -> Contract w s AuctionError Auction
startAuction StartAuctionParams{..} = do
    threadToken <- SM.getThreadToken
    logInfo $ "Obtained thread token: " <> Haskell.show threadToken
    let auction@Auction{..} = Auction {
        aProtocolToken = threadToken,
        aOwner = sapOwner,
        aAsset = sapAsset,
        aInitialPrice = sapInitialPrice,
        aEndTime = sapEndTime,
        aAuctionFee = sapAuctionFee
    }
    let inst         = typedValidator auction
        client       = machineClient inst auction

    _ <- handleError
            (\e -> do { logError (AuctionFailed e); throwError (StateMachineContractError e) })
            (SM.runInitialise client (initialState aOwner) aAsset)

    logInfo $ AuctionStarted auction
    pure auction

-- | Client code for the seller
payoutAuction :: Auction -> Contract w s AuctionError ()
payoutAuction auction = do
    let inst         = typedValidator auction
        client       = machineClient inst auction

    r <- SM.runStep client Payout
    case r of
        SM.TransitionFailure i            -> logError (TransitionFailed i)
        SM.TransitionSuccess (Finished h) -> logInfo $ AuctionEnded h
        SM.TransitionSuccess s            -> logWarn ("Unexpected state after Payout transition: " <> Haskell.show s)

cancelAuction :: Auction -> Contract w s AuctionError ()
cancelAuction auction = do
    let inst         = typedValidator auction
        client       = machineClient inst auction
    r <- SM.runStep client Cancel
    case r of
        SM.TransitionFailure i            -> logError (TransitionFailed i)
        SM.TransitionSuccess Canceled -> logInfo $ AuctionCanceled
        SM.TransitionSuccess s            -> logWarn ("Unexpected state after Payout transition: " <> Haskell.show s)

-- | Get the current state of the contract and log it.
currentState :: Auction -> Contract w s AuctionError (Maybe AuctionState)
currentState auction = mapError StateMachineContractError (SM.getOnChainState client) >>= \case
    Just (SM.OnChainState{SM.ocsTxOut=TypedScriptTxOut{tyTxOutData= s}}, _) -> do
        pure (Just s)
    _ -> do
        logWarn CurrentStateNotFound
        pure Nothing
    where
      inst         = typedValidator auction
      client       = machineClient inst auction

submitBid :: Auction -> Ada -> Contract w s AuctionError ((Either T.Text ()))
submitBid auction ada = do
    let inst         = typedValidator auction
        client       = machineClient inst auction
    self <- ownPubKeyHash
    let bid = Bid{newBid = ada, newBidder = self}
    result <- SM.runStep client bid
    case result of
        TransitionSuccess newState -> do
            logInfo @Haskell.String $ "Bid submitted. New state is: " <> Haskell.show newState
            pure $ Right ()
        _ -> do
            logInfo @Haskell.String $ "Auction bid failed"
            pure $ Left "Auction bid failed"

data AuctionLog =
    AuctionStarted Auction
    | AuctionFailed SM.SMContractError
    | BidSubmitted HighestBid
    | AuctionEnded HighestBid
    | AuctionCanceled
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
