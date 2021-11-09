{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFT.Validation (
  DatumNft (..),
  NftTrade,
  calculateShares,
  UserAct (..),
  asRedeemer,
  txPolicy,
  mkTxPolicy,
  txScrAddress,
  txValHash,
  nftCurrency,
  nftAsset,
  mintPolicy,
  mkMintPolicy,
  priceNotNegative,
  curSymbol,
) where

import PlutusTx.Prelude

import Plutus.V1.Ledger.Ada qualified as Ada (
  adaSymbol,
  adaToken,
  lovelaceValueOf,
 )

import Ledger (
  Address,
  AssetClass,
  CurrencySymbol,
  Datum (..),
  MintingPolicy,
  Redeemer (..),
  ScriptContext (..),
  TxInInfo (..),
  TxOut (..),
  ValidatorHash,
  Value,
  contains,
  findDatum,
  findOwnInput,
  from,
  getContinuingOutputs,
  mkMintingPolicyScript,
  ownCurrencySymbol,
  scriptContextTxInfo,
  scriptCurrencySymbol,
  to,
  txInInfoResolved,
  txInfoInputs,
  txInfoMint,
  txInfoOutputs,
  txInfoSignatories,
  txInfoValidRange,
  valuePaidTo,
 )

import Ledger.Typed.Scripts (
  DatumType,
  RedeemerType,
  TypedValidator,
  ValidatorTypes,
  mkTypedValidator,
  validatorAddress,
  validatorHash,
  wrapMintingPolicy,
  wrapValidator,
 )

import Data.Function (on)
import Ledger.Value (
  TokenName (..),
  assetClass,
  singleton,
  valueOf,
 )
import Plutus.V1.Ledger.Value (AssetClass (..), assetClassValueOf, isZero)
import PlutusTx qualified

import Data.Maybe (catMaybes)
import Mlabs.NFT.Types (
  AuctionBid (..),
  AuctionState (..),
  DatumNft (..),
  InformationNft (
    info'auctionState,
    info'author,
    info'id,
    info'owner,
    info'price,
    info'share
  ),
  MintAct (Initialise, Mint),
  NftAppInstance (appInstance'Address, appInstance'AppAssetClass),
  NftAppSymbol (app'symbol),
  NftId (nftId'contentHash),
  NftListHead (head'appInstance),
  NftListNode (node'appInstance, node'information, node'next),
  Pointer (pointer'assetClass),
  UserAct (..),
  UserId (getUserId),
  getAppInstance,
  getDatumPointer,
  nftTokenName,
 )

asRedeemer :: PlutusTx.ToData a => a -> Redeemer
asRedeemer = Redeemer . PlutusTx.toBuiltinData

{-# INLINEABLE mkMintPolicy #-}

-- | Minting policy for NFTs.
mkMintPolicy :: NftAppInstance -> MintAct -> ScriptContext -> Bool
mkMintPolicy !appInstance !act !ctx =
  case act of
    Mint nftid ->
      traceIfFalse "Only pointer of first node can change." firstChangedOnlyPtr
        && traceIfFalse "Exactly one NFT must be minted" (checkMintedAmount nftid)
        && traceIfFalse "Old first node must point to second node." (first `pointsTo'` second)
        && traceIfFalse "New first node must point to new node." (newFirst `pointsTo` newInserted)
        && traceIfFalse "New node must point to second node." (newInserted `pointsTo'` second)
        && traceIfFalse "New node must be smaller than second node." newIsSmallerThanSecond
        && traceIfFalse "New price cannot be negative." priceNotNegative'
        && traceIfFalse "Currency symbol must match app instance" checkCurrencySymbol
        && traceIfFalse "Minted token must be sent to script address" (checkSentAddress nftid)
        && traceIfFalse "Nodes must be sent to script address" checkNodesAddresses
        && traceIfFalse "Datum is not atttached to UTXo with correct Token" True -- todo
    Initialise ->
      traceIfFalse "The token is not present." True -- todo
        && traceIfFalse "Only One Unique Token Can be Minted" True -- todo
        && traceIfFalse "Only an Admin can initialise App." True -- todo
        && traceIfFalse "The token is not sent to the right address" True -- todo
  where
    ------------------------------------------------------------------------------
    -- Helpers

    !info = scriptContextTxInfo ctx
    !scriptAddress = appInstance'Address appInstance

    sentToScript TxOut {..} = txOutAddress == scriptAddress

    sort2 (x, y) = if x < y then (x, y) else (y, x)

    (newFirst, newInserted) = case getOutputDatums ctx of
      [x, y] -> sort2 (x, y)
      [_] -> traceError "Expected exactly two outputs with datums. Receiving one."
      [] -> traceError "Expected exactly two outputs with datums. Receiving none."
      _ -> traceError "Expected exactly two outputs with datums. Receiving more."

    first = case getInputDatums ctx of
      [x] -> x
      [] -> traceError "Expected exactly one input with datums. Receiving none."
      _ -> traceError "Expected exactly one input with datums. Receiving more."

    second = getDatumPointer first

    pointsTo d1 d2 = case (d1, d2) of
      (_, NodeDatum _) -> case getDatumPointer d1 of
        Just ptr -> (== nftTokenName d2) . snd . unAssetClass . pointer'assetClass $ ptr
        Nothing -> False
      _ -> False

    pointsTo' :: DatumNft -> Maybe Pointer -> Bool
    pointsTo' !datum !pointer = getDatumPointer datum == pointer

    ------------------------------------------------------------------------------
    -- Checks

    -- Check if nodes are sent back to script address
    checkNodesAddresses =
      let txs :: [TxOut] =
            fmap snd
              . mapMaybe (\(datum, tx) -> (,) <$> (PlutusTx.fromBuiltinData @DatumNft . getDatum $ datum) <*> pure tx)
              . mapMaybe (\(hash, tx) -> (,) <$> findDatum hash info <*> pure tx)
              . mapMaybe (\tx -> (,) <$> txOutDatumHash tx <*> pure tx)
              . txInfoOutputs
              . scriptContextTxInfo
              $ ctx
       in all sentToScript txs

    -- Check if price is positive
    priceNotNegative' = case newInserted of
      NodeDatum node -> priceNotNegative (info'price . node'information $ node)
      _ -> False

    -- Check if minted NFT is sent to script address
    checkSentAddress nftId =
      let currency = ownCurrencySymbol ctx
          tokenName = TokenName . nftId'contentHash $ nftId
       in maybe
            False
            sentToScript
            ( find (\TxOut {..} -> valueOf txOutValue currency tokenName == 1) $
                txInfoOutputs info
            )

    newIsSmallerThanSecond = case second of
      Nothing -> True
      Just ptr -> (> nftTokenName newInserted) . snd . unAssetClass . pointer'assetClass $ ptr

    -- Check if currency symbol is consistent
    checkCurrencySymbol =
      getAppInstance first == appInstance
        && getAppInstance newInserted == appInstance

    -- Check if minting only one token
    checkMintedAmount nftid =
      let currency = ownCurrencySymbol ctx
          tokenName = TokenName . nftId'contentHash $ nftid
       in valueOf (txInfoMint info) currency tokenName == 1

    -- Check if only thing changed in first node is `next` pointer
    firstChangedOnlyPtr = case (first, newFirst) of
      (NodeDatum node1, NodeDatum node2) ->
        node'appInstance node1 == node'appInstance node2
          && node'information node1 == node'information node2
      (HeadDatum node1, HeadDatum node2) ->
        head'appInstance node1 == head'appInstance node2
      _ -> False

mintPolicy :: NftAppInstance -> MintingPolicy
mintPolicy appInstance =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||wrapMintingPolicy . mkMintPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode appInstance

{-# INLINEABLE mkTxPolicy #-}

-- | A validator script for the user actions.
mkTxPolicy :: DatumNft -> UserAct -> ScriptContext -> Bool
mkTxPolicy !datum' !act !ctx =
  case datum' of
    HeadDatum headDat -> case act of
      MintAct {..} ->
        traceIfFalse "Proof Token must be paid back when using Head" (proofPaidBack act'nftId)
      -- must always pay back the proof Token. This happens when the Head datum is
      -- updated as the utxo needs to be consumed
      _ -> traceError "Cannot buy or set price of Head."
      where
        proofPaidBack _ =
          let (currency, tokenName) = unAssetClass . appInstance'AppAssetClass . head'appInstance $ headDat
              paysBack tx = valueOf (txOutValue tx) currency tokenName == 1
           in any paysBack . txInfoOutputs . scriptContextTxInfo $ ctx
    NodeDatum node ->
      traceIfFalse "NFT sent to wrong address." tokenSentToCorrectAddress
        && case act of
          MintAct {} ->
            traceIfFalse "Transaction can only use one NftListNode element as uniqueness proof." True -- todo
              && traceIfFalse "Transaction that uses Head as list proof must return it unchanged." True -- todo
              && traceIfFalse "Transaction can only mint one token." True -- todo
              && traceIfFalse "Not all used tokens are returned." True -- todo
              && traceIfFalse "Returned tokens have mismatching datum." True -- todo
              && traceIfFalse "Minted Token is not sent to correct address." True -- todo
          BuyAct {..} ->
            traceIfFalse "Transaction cannot mint." noMint
              && traceIfFalse "NFT not for sale." nftForSale
              && traceIfFalse "New Price cannot be negative." (priceNotNegative act'newPrice)
              && traceIfFalse "Act'Bid is too low for the NFT price." (bidHighEnough act'bid)
              && traceIfFalse "Datum is not consistent, illegaly altered." consistentDatumBuy
              && traceIfFalse "Only one Node is used in a Buy Action." True -- todo
              && traceIfFalse "Datum is not present in the correct UTXo." True -- todo
              && traceIfFalse "Token from input is not the same as Token in output" True -- todo
              && traceIfFalse "Not all used Tokens are returned." True -- todo
              && traceIfFalse "Returned Token UTXOs have mismatching datums." True -- todo
              && if ownerIsAuthor
                then traceIfFalse "Amount paid to author/owner does not match act'bid." (correctPaymentOnlyAuthor act'bid)
                else
                  traceIfFalse "Current owner is not paid their share." (correctPaymentOwner act'bid)
                    && traceIfFalse "Author is not paid their share." (correctPaymentAuthor act'bid)
          SetPriceAct {..} ->
            traceIfFalse "Transaction cannot mint." noMint
              && traceIfFalse "Datum does not correspond to NFTId, no datum is present, or more than one suitable datums are present." correctDatumSetPrice
              && traceIfFalse "New Price cannot be negative." (priceNotNegative act'newPrice)
              && traceIfFalse "Only owner exclusively can set NFT price." signedByOwner
              && traceIfFalse "Datum is not consistent, illegaly altered." consistentDatumSetPrice
              && traceIfFalse "Only one Node is used in a SetPrice Action." True -- todo
              && traceIfFalse "Datum is not present in the correct UTXo - lacking correct Token." True -- todo
              && traceIfFalse "Token from input is not the same as Token in output" True -- todo
              && traceIfFalse "Not all used Tokens are returned." True -- todo
              && traceIfFalse "Returned Token UTXOs have mismatching datums." True -- todo
          OpenAuctionAct {} ->
            traceIfFalse "Can't open auction: already in progress" noAuctionInProgress
              && traceIfFalse "Only owner can open auction" signedByOwner
              && traceIfFalse "Auction: datum illegally altered" auctionConsistentOpenDatum
          BidAuctionAct {..} ->
            traceIfFalse "Can't bid: No auction is in progress" (not noAuctionInProgress)
              && traceIfFalse "Auction bid is too low" (auctionBidHighEnough act'bid)
              && traceIfFalse "Auction deadline reached" correctAuctionBidSlotInterval
              && traceIfFalse "Auction: wrong input value" correctInputValue
              && traceIfFalse "Auction: datum illegally altered" (auctionConsistentDatum act'bid)
              && traceIfFalse "Auction bid value not supplied" (auctionBidValueSupplied act'bid)
              && traceIfFalse "Incorrect bid refund" correctBidRefund
          CloseAuctionAct {} ->
            traceIfFalse "Can't close auction: none in progress" (not noAuctionInProgress)
              && traceIfFalse "Auction deadline not yet reached" auctionDeadlineReached
              && traceIfFalse "Only owner can close auction" signedByOwner
              && traceIfFalse "Auction: new owner set incorrectly" auctionCorrectNewOwner
              && traceIfFalse "Auction: datum illegally altered" auctionConsistentCloseDatum
              && if ownerIsAuthor
                then traceIfFalse "Auction: amount paid to author/owner does not match bid" auctionCorrectPaymentOnlyAuthor
                else
                  traceIfFalse "Auction: owner not paid their share" auctionCorrectPaymentOwner
                    && traceIfFalse "Auction: author not paid their share" auctionCorrectPaymentAuthor
      where
        info = scriptContextTxInfo ctx

        !nInfo = node'information node
        oldDatum :: DatumNft = head . getInputDatums $ ctx

        oldNode :: NftListNode = case getNode oldDatum of
          Just n -> n
          Nothing -> traceError "Input datum is Head."

        !mauctionState = info'auctionState nInfo

        tokenValue :: Value
        tokenValue = singleton (app'symbol . act'symbol $ act) (nftTokenName datum') 1

        ------------------------------------------------------------------------------
        -- Utility functions.

        containsNft !v = valueOf v (app'symbol . act'symbol $ act) (nftTokenName datum') == 1

        !getAda = flip assetClassValueOf $ assetClass Ada.adaSymbol Ada.adaToken

        -- Check if the Person is being reimbursed accordingly, with the help of 2
        -- getter functions. Helper function.
        correctPayment !userIdGetter !shareCalcFn !bid = personGetsAda >= personWantsAda
          where
            personId = getUserId . userIdGetter $ node
            share = info'share . node'information $ node
            personGetsAda = getAda $ valuePaidTo info personId
            personWantsAda = getAda $ shareCalcFn bid share

        !ownerIsAuthor =
          (info'owner . node'information $ oldNode) == (info'author . node'information $ oldNode)

        getNode = \case
          NodeDatum n -> Just n
          _ -> Nothing

        withAuctionState f = maybe (traceError "Auction state expected") f mauctionState

        convDatum :: Datum -> Maybe DatumNft
        convDatum (Datum d) = PlutusTx.fromBuiltinData d

        newDatum :: DatumNft
        newDatum =
          case getContinuingOutputs ctx of
            [out] ->
              case txOutDatumHash out of
                Nothing -> traceError "getNextDatum: expected datum hash"
                Just dhash ->
                  case findDatum dhash info >>= convDatum of
                    Nothing -> traceError "getNextDatum: expected datum"
                    Just dt -> dt
            [] -> traceError "nextDatum: expected exactly one continuing output, got none"
            _ -> traceError "nextDatum: expected exactly one continuing output, got several instead"

        newNodeInfo :: InformationNft
        newNodeInfo =
          case newDatum of
            HeadDatum _ -> traceError "nextNodeInfo: expected NodeDatum, got HeadDatum instead"
            NodeDatum listNode -> node'information listNode

        ------------------------------------------------------------------------------
        -- Checks

        -- Check whether there's auction in progress and disallow buy/setprice actions.
        noAuctionInProgress :: Bool
        noAuctionInProgress = isNothing mauctionState

        auctionBidHighEnough :: Integer -> Bool
        auctionBidHighEnough amount =
          withAuctionState $ \auctionState ->
            case as'highestBid auctionState of
              Nothing -> amount >= as'minBid auctionState
              Just highestBid -> amount > ab'bid highestBid

        correctAuctionBidSlotInterval :: Bool
        correctAuctionBidSlotInterval =
          withAuctionState $ \auctionState ->
            to (as'deadline auctionState) `contains` txInfoValidRange info

        auctionDeadlineReached :: Bool
        auctionDeadlineReached =
          withAuctionState $ \auctionState ->
            from (as'deadline auctionState) `contains` txInfoValidRange info

        auctionCorrectPayment :: (Integer -> Bool) -> Bool
        auctionCorrectPayment correctPaymentCheck =
          withAuctionState $ \auctionState ->
            case as'highestBid auctionState of
              Nothing -> True
              Just (AuctionBid bid _bidder) ->
                correctPaymentCheck bid

        auctionCorrectPaymentOwner :: Bool
        auctionCorrectPaymentOwner = auctionCorrectPayment correctPaymentOwner

        auctionCorrectPaymentAuthor :: Bool
        auctionCorrectPaymentAuthor = auctionCorrectPayment correctPaymentAuthor

        auctionCorrectPaymentOnlyAuthor :: Bool
        auctionCorrectPaymentOnlyAuthor =
          withAuctionState $ \auctionState ->
            case as'highestBid auctionState of
              Nothing -> True
              Just (AuctionBid bid _) ->
                correctPaymentOnlyAuthor bid

        correctBidRefund :: Bool
        correctBidRefund =
          withAuctionState $ \auctionState ->
            case as'highestBid auctionState of
              Nothing -> True
              Just (AuctionBid bid bidder) ->
                valuePaidTo info (getUserId bidder) == Ada.lovelaceValueOf bid

        correctInputValue :: Bool
        correctInputValue =
          case findOwnInput ctx of
            Nothing -> traceError "findOwnInput: Nothing"
            Just (TxInInfo _ out) ->
              case mauctionState of
                Nothing -> traceError "mauctionState: Nothing"
                Just as -> case as'highestBid as of
                  Nothing -> tokenValue == txOutValue out
                  Just hb -> txOutValue out == (tokenValue <> Ada.lovelaceValueOf (ab'bid hb))

        auctionBidValueSupplied :: Integer -> Bool
        auctionBidValueSupplied redeemerBid =
          case getContinuingOutputs ctx of
            [out] -> txOutValue out == tokenValue <> Ada.lovelaceValueOf redeemerBid
            [] -> traceError "auctionBidValueSupplied: expected exactly one continuing output, got none"
            _ -> traceError "auctionBidValueSupplied: expected exactly one continuing output, got several instead"

        auctionCorrectNewOwner :: Bool
        auctionCorrectNewOwner =
          withAuctionState $ \auctionState ->
            case as'highestBid auctionState of
              Nothing -> True
              Just (AuctionBid _ bidder) ->
                bidder == newOwner
          where
            newOwner = info'owner newNodeInfo

        auctionConsistentCloseDatum :: Bool
        auctionConsistentCloseDatum =
          -- Checking that all fields remain the same except owner
          info'id newNodeInfo == info'id nInfo
            && info'share newNodeInfo == info'share nInfo
            && info'author newNodeInfo == info'author nInfo
            && info'price newNodeInfo == info'price nInfo
            && checkOwner
          where
            checkOwner = withAuctionState $ \auctionState ->
              case as'highestBid auctionState of
                Nothing -> info'owner newNodeInfo == info'owner nInfo
                _ -> True

        auctionConsistentOpenDatum :: Bool
        auctionConsistentOpenDatum =
          -- Checking that all fields remain the same except auctionState
          info'id newNodeInfo == info'id nInfo
            && info'share newNodeInfo == info'share nInfo
            && info'author newNodeInfo == info'author nInfo
            && info'owner newNodeInfo == info'owner nInfo
            && info'price newNodeInfo == info'price nInfo

        auctionConsistentDatum :: Integer -> Bool
        auctionConsistentDatum redeemerBid =
          let checkAuctionState =
                case (info'auctionState newNodeInfo, info'auctionState nInfo) of
                  ( Just (AuctionState _ nextDeadline nextMinBid)
                    , Just (AuctionState _ deadline minBid)
                    ) ->
                      nextDeadline == deadline && nextMinBid == minBid
                  _ -> traceError "auctionConsistentDatum (checkAauctionState): expected auction state"

              checkHighestBid =
                case (info'auctionState newNodeInfo, info'auctionState nInfo) of
                  ( Just (AuctionState (Just (AuctionBid nextBid _)) _ _)
                    , Just (AuctionState (Just (AuctionBid bid _)) _ _)
                    ) ->
                      nextBid > bid && nextBid == redeemerBid
                  ( Just (AuctionState (Just (AuctionBid nextBid _)) _ _)
                    , Just (AuctionState Nothing _ minBid)
                    ) ->
                      nextBid >= minBid && nextBid == redeemerBid
                  _ -> traceError "auctionConsistentDatum (checkHighestBid): expected auction state"
           in info'id newNodeInfo == info'id nInfo
                && info'share newNodeInfo == info'share nInfo
                && info'author newNodeInfo == info'author nInfo
                && info'owner newNodeInfo == info'owner nInfo
                && info'price newNodeInfo == info'price nInfo
                && checkAuctionState
                && checkHighestBid

        -- Check if changed only owner and price
        !consistentDatumBuy =
          on (==) node'next oldNode node
            && on (==) node'appInstance oldNode node
            && on (==) (info'author . node'information) oldNode node
            && on (==) (info'share . node'information) oldNode node
            && on (==) (info'id . node'information) oldNode node

        -- Check if nft is for sale (price is not Nothing)
        !nftForSale = isJust . info'price . node'information $ oldNode

        -- Check if author of NFT receives share
        !correctPaymentAuthor = correctPayment (info'author . node'information) calculateAuthorShare

        -- Check if owner of NFT receives share
        !correctPaymentOwner = correctPayment (info'owner . node'information) calculateOwnerShare

        -- Check if author of NFT receives share when is also owner
        correctPaymentOnlyAuthor !bid = authorGetsAda >= bid
          where
            author = getUserId . info'author . node'information $ node
            authorGetsAda = getAda $ valuePaidTo info author

        -- Check if buy bid is higher or equal than price
        bidHighEnough !bid = case info'price . node'information $ oldNode of
          Nothing -> False -- NFT not for sale.
          Just price -> price <= bid

        -- Check if the datum attached is also present in the set price transaction.
        !correctDatumSetPrice =
          let nodes :: [NftListNode] = mapMaybe getNode . getInputDatums $ ctx
              suitableDatums = filter (== info'id nInfo) . fmap (info'id . node'information) $ nodes
           in case suitableDatums of
                [_] -> True
                _ -> False

        -- Check if only thing changed in nodes is price
        !consistentDatumSetPrice =
          on (==) node'next oldNode node
            && on (==) node'appInstance oldNode node
            && on (==) (info'author . node'information) oldNode node
            && on (==) (info'owner . node'information) oldNode node
            && on (==) (info'share . node'information) oldNode node
            && on (==) (info'id . node'information) oldNode node

        -- Check if the price of NFT is changed by the owner of NFT
        !signedByOwner =
          case txInfoSignatories $ scriptContextTxInfo ctx of
            [pkh] -> pkh == getUserId (info'owner $ node'information node)
            _ -> False

        -- Check if no new token is minted.
        !noMint = isZero . txInfoMint . scriptContextTxInfo $ ctx

        -- Check if the NFT is sent to the correct address.
        !tokenSentToCorrectAddress =
          let sentBack tx = txOutAddress tx == (appInstance'Address . node'appInstance $ oldNode)
           in all sentBack $ filter (containsNft . txOutValue) (txInfoOutputs . scriptContextTxInfo $ ctx)

{-# INLINEABLE catMaybes' #-}
catMaybes' :: [Maybe a] -> [a]
catMaybes' = catMaybes

{-# INLINEABLE priceNotNegative #-}
priceNotNegative :: Maybe Integer -> Bool
priceNotNegative = maybe True (>= 0)

data NftTrade
instance ValidatorTypes NftTrade where
  type DatumType NftTrade = DatumNft
  type RedeemerType NftTrade = UserAct

{-# INLINEABLE txPolicy #-}
txPolicy :: TypedValidator NftTrade
txPolicy =
  mkTypedValidator @NftTrade
    $$(PlutusTx.compile [||mkTxPolicy||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = wrapValidator @DatumNft @UserAct

{-# INLINEABLE txValHash #-}
txValHash :: ValidatorHash
txValHash = validatorHash txPolicy

{-# INLINEABLE txScrAddress #-}
txScrAddress :: Ledger.Address
txScrAddress = validatorAddress txPolicy

{-# INLINEABLE curSymbol #-}

-- | Calculate the currency symbol of the NFT.
curSymbol :: NftAppInstance -> CurrencySymbol
curSymbol appInstance = scriptCurrencySymbol $ mintPolicy appInstance

{-# INLINEABLE nftCurrency #-}

-- | Calculate the NFT `CurrencySymbol` from NftId.
nftCurrency :: DatumNft -> CurrencySymbol
nftCurrency = \case
  HeadDatum x -> curSymbol $ head'appInstance x
  NodeDatum x -> curSymbol $ node'appInstance x

{-# INLINEABLE nftAsset #-}

-- | Calculate the NFT `AssetClass` from Datum.
nftAsset :: DatumNft -> AssetClass
nftAsset datum =
  AssetClass
    ( nftCurrency datum
    , nftTokenName datum
    )

{-# INLINEABLE calculateShares #-}

{- | Returns the amount each party should be paid given the number of shares
 retained by author.
-}
calculateShares :: Integer -> Rational -> (Value, Value)
calculateShares bid authorShare = (toOwner, toAuthor)
  where
    authorPart = round $ fromInteger bid * authorShare
    toAuthor = Ada.lovelaceValueOf authorPart
    toOwner = Ada.lovelaceValueOf $ bid - authorPart

{-# INLINEABLE calculateOwnerShare #-}

-- | Returns the calculated value of shares.
calculateOwnerShare :: Integer -> Rational -> Value
calculateOwnerShare x y = fst $ calculateShares x y

{-# INLINEABLE calculateAuthorShare #-}

-- | Returns the calculated value of shares.
calculateAuthorShare :: Integer -> Rational -> Value
calculateAuthorShare x y = snd $ calculateShares x y

{-# INLINEABLE getInputDatums #-}

-- | Retuns datums attached to inputs of transaction
getInputDatums :: PlutusTx.FromData a => ScriptContext -> [a]
getInputDatums ctx =
  mapMaybe (PlutusTx.fromBuiltinData . getDatum)
    . mapMaybe (\hash -> findDatum hash $ scriptContextTxInfo ctx)
    . mapMaybe (txOutDatumHash . txInInfoResolved)
    . txInfoInputs
    . scriptContextTxInfo
    $ ctx

{-# INLINEABLE getOutputDatums #-}

-- | Retuns datums attached to outputs of transaction
getOutputDatums :: PlutusTx.FromData a => ScriptContext -> [a]
getOutputDatums ctx =
  mapMaybe (PlutusTx.fromBuiltinData . getDatum)
    . mapMaybe (\hash -> findDatum hash $ scriptContextTxInfo ctx)
    . mapMaybe txOutDatumHash
    . txInfoOutputs
    . scriptContextTxInfo
    $ ctx
