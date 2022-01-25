{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
-- FIXME: Remove after uncommenting commented parts
{-# OPTIONS_GHC -Wno-unused-imports #-}

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

import PlutusTx qualified
import PlutusTx.Prelude

import Ledger (
  Datum (..),
  MintingPolicy,
  Redeemer (..),
  ValidatorHash,
  contains,
  findOwnInput,
  from,
  mkMintingPolicyScript,
  mkValidatorScript,
  scriptCurrencySymbol,
  to,
 )
import Ledger.Typed.Scripts (
  DatumType,
  RedeemerType,
  TypedValidator,
  ValidatorTypes,
  WrappedMintingPolicyType,
  mkTypedValidator,
  unsafeMkTypedValidator,
  validatorAddress,
  validatorHash,
  wrapMintingPolicy,
 )
import Ledger.Typed.TypeUtils (Any)

import Data.Function (on)
import Data.Maybe (catMaybes)
import Data.Tuple.Extra (uncurry3)

import Mlabs.NFT.Governance.Types (
  GovDatum (gov'list),
  GovLHead (GovLHead, govLHead'feeRate, govLHead'pkh),
  LList (HeadLList, _head'info, _head'next),
 )
import Mlabs.NFT.Spooky (
  Address,
  AssetClass (AssetClass),
  CurrencySymbol,
  ScriptContext,
  TokenName (TokenName),
  TxOut,
  Value,
  adaSymbol,
  adaToken,
  assetClass,
  assetClassValueOf,
  findDatum,
  flattenValue,
  lovelaceValueOf,
  ownCurrencySymbol,
  scriptContextTxInfo,
  singleton,
  toSpooky,
  toSpookyAddress,
  toSpookyCurrencySymbol,
  toSpookyTokenName,
  txInInfoResolved,
  txInfoInputs,
  txInfoMint,
  txInfoOutputs,
  txInfoSignatories,
  txOutAddress,
  txOutDatumHash,
  txOutValue,
  unAssetClass,
  unPaymentPubKeyHash,
  unSpookyCurrencySymbol,
  unSpookyTokenName,
  unTokenName,
  valueOf,
  valuePaidTo,
 )
import Mlabs.NFT.Types (
  DatumNft (..),
  InformationNft (info'author', info'price'),
  MintAct (Initialise, Mint),
  NftAppInstance,
  NftId (NftId),
  NftListHead,
  NftListNode (node'information'),
  Pointer,
  UniqueToken,
  UserAct (..),
  UserId (UserId),
  act'bid,
  act'newPrice,
  act'symbol,
  app'symbol,
  appInstance'Address,
  appInstance'Admins,
  appInstance'UniqueToken,
  getAppInstance,
  getDatumPointer,
  getUserId,
  head'appInstance,
  info'author,
  info'id,
  info'owner,
  info'price,
  info'share,
  mint'nftId,
  nftId'contentHash,
  nftTokenName,
  node'appInstance,
  node'information,
  pointer'assetClass,
 )

asRedeemer :: PlutusTx.ToData a => a -> Redeemer
asRedeemer = Redeemer . PlutusTx.toBuiltinData

{-# INLINEABLE mkMintPolicy #-}

-- | Minting policy for NFTs.
mkMintPolicy :: NftAppInstance -> MintAct -> ScriptContext -> Bool
mkMintPolicy !appInstance !act !ctx =
  case act of
    mintAct@Mint {} ->
      traceIfFalse' "Only pointer of first node can change." firstChangedOnlyPtr
        && traceIfFalse' "Exactly one NFT must be minted" (checkMintedAmount . mint'nftId $ mintAct)
        && traceIfFalse' "Old first node must point to second node." (first `pointsTo'` second)
        && traceIfFalse' "New first node must point to new node." (newFirst `pointsTo` newInserted)
        && traceIfFalse' "New node must point to second node." (newInserted `pointsTo'` second)
        && traceIfFalse' "New node must be smaller than second node." newIsSmallerThanSecond
        && traceIfFalse' "New price cannot be negative." priceNotNegative'
        && traceIfFalse' "Currency symbol must match app instance" checkCurrencySymbol
        && traceIfFalse' "Minted token must be sent to script address" (checkSentAddress . mint'nftId $ mintAct)
        && traceIfFalse' "Nodes must be sent to script address" checkNodesAddresses
        && traceIfFalse' "Datum is not atttached to UTXo with correct Token" (checkAttachedDatum . mint'nftId $ mintAct)
    Initialise ->
      traceIfFalse' "The token is not present." headTokenIsPresent
        && traceIfFalse' "Only one Unique Token can be minted" headTokenIsUnique
        && traceIfFalse' "The token is not sent to the right address" headTokenToRightAddress
        && traceIfFalse' "Only an admin can initialise app." checkAdminSig
  where
    ------------------------------------------------------------------------------
    -- Helpers

    !info = scriptContextTxInfo ctx
    !scriptAddress = appInstance'Address appInstance

    sentToScript tx = txOutAddress tx == scriptAddress

    sort2 (x, y) = if x < y then (x, y) else (y, x)

    (newFirst, newInserted) = case getOutputDatums ctx of
      [x, y] -> sort2 (x, y)
      [_] -> traceError' "Expected exactly two outputs with datums. Receiving one."
      [] -> traceError' "Expected exactly two outputs with datums. Receiving none."
      _ -> traceError' "Expected exactly two outputs with datums. Receiving more."
    first = case getInputDatums ctx of
      [x] -> x
      [] -> traceError' "Expected exactly one input with datums. Receiving none."
      _ -> traceError' "Expected exactly one input with datums. Receiving more."
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
              . getOutputDatumsWithTx @DatumNft
              $ ctx
       in all sentToScript txs

    -- Check if price is positive
    priceNotNegative' = case newInserted of
      NodeDatum node -> priceNotNegative (info'price . node'information $ node)
      _ -> False

    -- Check if minted NFT is sent to script address
    checkSentAddress nftId =
      let currency = ownCurrencySymbol ctx
          tokenName = TokenName . toSpooky . nftId'contentHash $ nftId
          txOut = find (\tx -> valueOf (txOutValue tx) currency tokenName == 1) $ txInfoOutputs info
       in maybe False sentToScript txOut

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
          tokenName = TokenName . toSpooky . nftId'contentHash $ nftid
       in txInfoMint info == singleton currency tokenName 1

    -- Check if only thing changed in first node is `next` pointer
    firstChangedOnlyPtr = case (first, newFirst) of
      (NodeDatum node1, NodeDatum node2) ->
        node'appInstance node1 == node'appInstance node2
          && node'information node1 == node'information node2
      (HeadDatum node1, HeadDatum node2) ->
        head'appInstance node1 == head'appInstance node2
      _ -> False

    -- Check if Datum and Token id matches
    checkAttachedDatum nftId =
      let snd3 (_, y, _) = y
          mintedId =
            NftId
              . toSpooky
              . unTokenName
              . snd3
              . head
              . flattenValue
              . txInfoMint
              . scriptContextTxInfo
              $ ctx
       in case newInserted of
            HeadDatum _ -> False
            NodeDatum node ->
              let datumId = info'id . node'information $ node
               in mintedId == datumId && datumId == nftId

    !outputsWithHeadDatum =
      filter
        ( \(datum, _) ->
            case datum of
              HeadDatum _ -> True
              _ -> False
        )
        $ getOutputDatumsWithTx ctx

    -- Check if the head token is present
    headTokenIsPresent =
      let validValue (sym, _, _) = sym == ownCurrencySymbol ctx
          validHeadToken tx = any validValue $ flattenValue . txOutValue $ tx
       in any (validHeadToken . snd) outputsWithHeadDatum

    -- Check if the head token is spent to the right address
    headTokenToRightAddress =
      let validValue (sym, _, _) = sym == ownCurrencySymbol ctx
          validHeadToken tx =
            sentToScript tx
              && any validValue (flattenValue . txOutValue $ tx)
       in any (validHeadToken . snd) outputsWithHeadDatum

    -- Check the uniqueness of minted head token
    headTokenIsUnique =
      let validValue (sym, _, v) = sym == ownCurrencySymbol ctx && v == 1
          validHeadToken tx =
            sentToScript tx
              && any validValue (flattenValue . txOutValue $ tx)
       in any (validHeadToken . snd) outputsWithHeadDatum

    -- Check an admin signed the transaction
    checkAdminSig =
      let admins = appInstance'Admins appInstance
       in any (`elem` admins) $ UserId . toSpooky <$> txInfoSignatories info

mintPolicy :: NftAppInstance -> MintingPolicy
mintPolicy appInstance =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||myWrapMintingPolicy . mkMintPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode appInstance

{-# INLINEABLE mkTxPolicy #-}

-- | A validator script for the user actions.
mkTxPolicy :: UniqueToken -> DatumNft -> UserAct -> ScriptContext -> Bool
mkTxPolicy _ !datum' !act !ctx =
  case act of
    MintAct {} -> case datum' of
      NodeDatum _ ->
        traceIfFalse' "Transaction can only use one NftListNode element as uniqueness proof." onlyOneNodeAttached
          && traceIfFalse' "Not all used tokens are returned." checkTokenReturned
          && traceIfFalse' "Returned Token UTXOs have mismatching datums." checkMissMatchDatumMint
      HeadDatum headDat ->
        -- must always pay back the proof Token. This happens when the Head datum is
        -- updated as the utxo needs to be consumed
        traceIfFalse' "Proof Token must be paid back when using Head" proofPaidBack
          && traceIfFalse' "Transaction that uses Head as list proof must return it unchanged." headUnchanged
        where
          oldHead :: NftListHead = case mapMaybe getHead . getInputDatums $ ctx of
            [] -> traceError' "oldHead: Head not found"
            [h] -> h
            _ -> traceError' "oldHead: More than one head"

          !proofPaidBack = any paysBack . txInfoOutputs . scriptContextTxInfo $ ctx
            where
              (currency, tokenName) = unAssetClass . appInstance'UniqueToken . head'appInstance $ headDat
              paysBack tx = valueOf (txOutValue tx) currency tokenName == 1
          !headUnchanged = oldHead == headDat
    buyAct@BuyAct {} -> case datum' of
      NodeDatum node ->
        traceIfFalse' "Transaction cannot mint." noMint
          && traceIfFalse' "NFT not for sale." nftForSale
          && traceIfFalse' "New Price cannot be negative." (priceNotNegative . act'newPrice $ buyAct)
          && traceIfFalse' "Act'Bid is too low for the NFT price." (bidHighEnough . act'bid $ buyAct)
          && traceIfFalse' "Datum is not consistent, illegally altered." (consistentDatumBuy node)
          && traceIfFalse' "Only one Node must be used in a Buy Action." onlyOneNodeAttached
          && traceIfFalse' "Not all used Tokens are returned." checkTokenReturned
          && traceIfFalse' "Returned Token UTXO has mismatching datum." checkMissMatchDatum
          && if ownerIsAuthor
            then traceIfFalse' "Amount paid to author/owner does not match" (correctPaymentOnlyAuthor node . act'bid $ buyAct)
            else
              traceIfFalse' "Current owner is not paid their share." (correctPaymentOwner node . act'bid $ buyAct)
                && traceIfFalse' "Author is not paid their share." (correctPaymentAuthor node . act'bid $ buyAct)
      HeadDatum _ -> False
    setPriceAct@SetPriceAct {} -> case datum' of
      NodeDatum node ->
        traceIfFalse' "Transaction cannot mint." noMint
          && traceIfFalse' "Datum does not correspond to NFTId, no datum is present, or more than one suitable datums are present." (correctDatumSetPrice node)
          && traceIfFalse' "New Price cannot be negative." (priceNotNegative . act'newPrice $ setPriceAct)
          && traceIfFalse' "Only owner exclusively can set NFT price." (signedByOwner node)
          && traceIfFalse' "Datum is not consistent, illegally altered." (consistentDatumSetPrice node)
          && traceIfFalse' "Only one Node must be used in a SetPrice Action." onlyOneNodeAttached
          && traceIfFalse' "Not all used Tokens are returned." checkTokenReturned
          && traceIfFalse' "Returned Token UTXO has mismatching datum." checkMissMatchDatum
      -- && traceIfFalse' "NFT is on auction" (checkIsNotOnAuction node)
      HeadDatum _ -> False
    _ -> False
  where
    -- OpenAuctionAct {} -> case datum' of
    --   NodeDatum node ->
    --     traceIfFalse' "Can't open auction: already in progress" (noAuctionInProgress node)
    --       && traceIfFalse' "Only owner can open auction" (signedByOwner node)
    --       && traceIfFalse' "Open Auction: datum illegally altered" (auctionConsistentOpenDatum node)
    --       && traceIfFalse' "NFT price must be set to Nothing" checkPriceIsNothing
    --   HeadDatum _ -> False
    -- BidAuctionAct {..} -> case datum' of
    --   NodeDatum node ->
    --     traceIfFalse' "Can't bid: No auction is in progress" (not $ noAuctionInProgress node)
    --       && traceIfFalse' "Auction bid is too low" (auctionBidHighEnough node act'bid)
    --       && traceIfFalse' "Auction deadline reached" (correctAuctionBidSlotInterval node)
    --       && traceIfFalse' "Auction: wrong input value" (correctInputValue node)
    --       && traceIfFalse' "Bid Auction: datum illegally altered" (auctionConsistentDatum node act'bid)
    --       && traceIfFalse' "Auction bid value not supplied" (auctionBidValueSupplied act'bid)
    --       && traceIfFalse' "Incorrect bid refund" (correctBidRefund node)
    --   HeadDatum _ -> False
    -- CloseAuctionAct {} -> case datum' of
    --   NodeDatum node ->
    --     traceIfFalse' "Can't close auction: none in progress" (not $ noAuctionInProgress node)
    --       && traceIfFalse' "Auction deadline not yet reached" (auctionDeadlineReached node)
    --       && traceIfFalse' "Auction: new owner set incorrectly" (auctionCorrectNewOwner node)
    --       && traceIfFalse' "Close Auction: datum illegally altered" (auctionConsistentCloseDatum node)
    --       && if ownerIsAuthor
    --         then traceIfFalse' "Auction: amount paid to author/owner does not match bid" (auctionCorrectPaymentOnlyAuthor node)
    --         else
    --           traceIfFalse' "Auction: owner not paid their share" (auctionCorrectPaymentOwner node)
    --             && traceIfFalse' "Auction: author not paid their share" (auctionCorrectPaymentAuthor node)
    --   HeadDatum _ -> False

    info = scriptContextTxInfo ctx

    !nInfo = node'information

    oldNode :: NftListNode = case mapMaybe getNode . getInputDatums $ ctx of
      [n] -> n
      _ -> traceError' "Input datum not found."

    -- mauctionState = info'auctionState . nInfo

    -- tokenValue :: Value
    -- tokenValue = singleton (app'symbol . act'symbol $ act) (nftTokenName datum') 1

    ------------------------------------------------------------------------------
    -- Utility functions.
    nftCurr = app'symbol . act'symbol $ act

    feeRate :: Rational
    feeRate = 5 % 1000
    --  | [GovLHead {..}] <- mapMaybe (getGHead . gov'list . fst) $ getOutputDatumsWithTx @GovDatum ctx =
    --    govLHead'feeRate
    --  | otherwise = traceError' "Expecting excatly one gov head"
    --  where
    --    getGHead HeadLList {..} = Just _head'info
    --    getGHead _ = Nothing

    getHead h = case h of
      HeadDatum h' -> Just h'
      _ -> Nothing

    getNode n = case n of
      NodeDatum n' -> Just n'
      _ -> Nothing

    subtractFee price = price - calcFee price

    calcFee price = round (fromInteger price * feeRate)

    sort2On f (x, y) = if f x < f y then (x, y) else (y, x)

    fst3 (x, _, _) = x

    -- containsNft !v = valueOf v nftCurr (nftTokenName datum') == 1

    !getAda = flip assetClassValueOf $ assetClass adaSymbol adaToken

    -- Check if the Person is being reimbursed accordingly, with the help of 2
    -- getter functions. Helper function.
    correctPayment node !userIdGetter !shareCalcFn !bid = personGetsAda >= personWantsAda
      where
        personId = getUserId . userIdGetter $ node
        share = info'share . node'information $ node
        personGetsAda = getAda $ valuePaidTo info (unPaymentPubKeyHash personId)
        personWantsAda = subtractFee . getAda $ shareCalcFn bid share

    ownerIsAuthor =
      (info'owner . node'information $ oldNode) == (info'author . node'information $ oldNode)

    -- withAuctionState node f = maybe (traceError "Auction state expected") f (mauctionState node)

    -- newDatum = case getOutputDatums ctx of
    -- [x] -> x
    -- [] -> error ()
    -- _ -> error ()

    -- newNodeInfo :: InformationNft
    -- newNodeInfo =
    -- case newDatum of
    -- HeadDatum _ -> error () -- traceError "nextNodeInfo: expected NodeDatum, got HeadDatum instead"
    -- NodeDatum listNode -> node'information listNode

    -- Check if Datum id matches NFT id in UTXO
    checkTxDatumMatch nodeDatum tx =
      let cur = app'symbol . act'symbol $ act
          tn = TokenName . toSpooky . nftId'contentHash . info'id . node'information $ nodeDatum
       in valueOf (txOutValue tx) cur tn == 1

    fromJust !x = fromMaybe (traceError' "fromJust") x

    ------------------------------------------------------------------------------
    -- Checks
    extractCurr c =
      mconcat
        . fmap (uncurry3 singleton)
        . filter ((== c) . fst3)
        . flattenValue

    -- Check whether there's auction in progress and disallow buy/setprice actions.
    -- noAuctionInProgress :: NftListNode -> Bool
    -- noAuctionInProgress = isNothing . mauctionState

    -- auctionBidHighEnough :: NftListNode -> Integer -> Bool
    -- auctionBidHighEnough node amount =
    --   withAuctionState node $ \auctionState ->
    --     case as'highestBid auctionState of
    --       Nothing -> amount >= as'minBid auctionState
    --       Just highestBid -> amount > ab'bid highestBid

    -- correctAuctionBidSlotInterval :: NftListNode -> Bool
    -- correctAuctionBidSlotInterval node =
    --   withAuctionState node $ \auctionState ->
    --     to (as'deadline auctionState) `contains` txInfoValidRange info

    -- auctionDeadlineReached :: NftListNode -> Bool
    -- auctionDeadlineReached node =
    --   withAuctionState node $ \auctionState ->
    --     from (as'deadline auctionState) `contains` txInfoValidRange info

    -- auctionCorrectPayment :: NftListNode -> (Integer -> Bool) -> Bool
    -- auctionCorrectPayment node correctPaymentCheck =
    --   withAuctionState node $ \auctionState ->
    --     case as'highestBid auctionState of
    --       Nothing -> True
    --       Just (AuctionBid bid _bidder) ->
    --         correctPaymentCheck bid

    -- auctionCorrectPaymentOwner :: NftListNode -> Bool
    -- auctionCorrectPaymentOwner node = auctionCorrectPayment node (correctPaymentOwner node)

    -- auctionCorrectPaymentAuthor :: NftListNode -> Bool
    -- auctionCorrectPaymentAuthor node = auctionCorrectPayment node (correctPaymentAuthor node)

    -- auctionCorrectPaymentOnlyAuthor :: NftListNode -> Bool
    -- auctionCorrectPaymentOnlyAuthor node =
    --   withAuctionState node $ \auctionState ->
    --     case as'highestBid auctionState of
    --       Nothing -> True
    --       Just (AuctionBid bid _) ->
    --         correctPaymentOnlyAuthor node bid

    -- correctBidRefund :: NftListNode -> Bool
    -- correctBidRefund node =
    --   withAuctionState node $ \auctionState ->
    --     case as'highestBid auctionState of
    --       Nothing -> True
    --       Just (AuctionBid bid bidder) ->
    --         valuePaidTo info (getUserId bidder) == lovelaceValueOf bid

    -- correctInputValue :: NftListNode -> Bool
    -- correctInputValue node =
    --   case findOwnInput ctx of
    --     Nothing -> traceError "findOwnInput: Nothing"
    --     Just (TxInInfo _ out) ->
    --       case mauctionState node of
    --         Nothing -> traceError "mauctionState: Nothing"
    --         Just as -> case as'highestBid as of
    --           Nothing -> tokenValue == txOutValue out
    --           Just hb -> txOutValue out == (tokenValue <> lovelaceValueOf (ab'bid hb))

    -- auctionBidValueSupplied :: Integer -> Bool
    -- auctionBidValueSupplied redeemerBid =
    --   case fmap snd . getOutputDatumsWithTx @DatumNft $ ctx of
    --     [out] -> txOutValue out == tokenValue <> lovelaceValueOf redeemerBid
    --     [] -> traceError "auctionBidValueSupplied: expected exactly one continuing output, got none"
    --     _ -> traceError "auctionBidValueSupplied: expected exactly one continuing output, got several instead"

    -- auctionCorrectNewOwner :: NftListNode -> Bool
    -- auctionCorrectNewOwner node =
    --   withAuctionState node $ \auctionState ->
    --     case as'highestBid auctionState of
    --       Nothing -> True
    --       Just (AuctionBid _ bidder) ->
    --         bidder == newOwner
    --   where
    --     newOwner = info'owner newNodeInfo

    -- auctionConsistentCloseDatum :: NftListNode -> Bool
    -- auctionConsistentCloseDatum node =
    --   -- Checking that all fields remain the same except owner
    --   info'id newNodeInfo == info'id nInfo'
    --     && info'share newNodeInfo == info'share nInfo'
    --     && info'author newNodeInfo == info'author nInfo'
    --     && info'price newNodeInfo == info'price nInfo'
    --     && checkOwner
    --   where
    --     nInfo' = nInfo node

    --     checkOwner = withAuctionState node $ \auctionState ->
    --       case as'highestBid auctionState of
    --         Nothing -> info'owner newNodeInfo == info'owner nInfo'
    --         _ -> True

    -- auctionConsistentOpenDatum :: NftListNode -> Bool
    -- auctionConsistentOpenDatum node =
    --   -- Checking that all fields remain the same except auctionState
    --   info'id newNodeInfo == info'id nInfo'
    --     && info'share newNodeInfo == info'share nInfo'
    --     && info'author newNodeInfo == info'author nInfo'
    --     && info'owner newNodeInfo == info'owner nInfo'
    --   where
    --     nInfo' = nInfo node

    -- checkPriceIsNothing = isNothing . info'price $ newNodeInfo

    -- auctionConsistentDatum :: NftListNode -> Integer -> Bool
    -- auctionConsistentDatum node redeemerBid =
    --   let nInfo' = nInfo node
    --       checkAuctionState =
    --         case (info'auctionState newNodeInfo, info'auctionState nInfo') of
    --           ( Just (AuctionState _ nextDeadline nextMinBid)
    --             , Just (AuctionState _ deadline minBid)
    --             ) ->
    --               nextDeadline == deadline && nextMinBid == minBid
    --           _ -> traceError "auctionConsistentDatum (checkAauctionState): expected auction state"

    --       checkHighestBid =
    --         case (info'auctionState newNodeInfo, info'auctionState nInfo') of
    --           ( Just (AuctionState (Just (AuctionBid nextBid _)) _ _)
    --             , Just (AuctionState (Just (AuctionBid bid _)) _ _)
    --             ) ->
    --               nextBid > bid && nextBid == redeemerBid
    --           ( Just (AuctionState (Just (AuctionBid nextBid _)) _ _)
    --             , Just (AuctionState Nothing _ minBid)
    --             ) ->
    --               nextBid >= minBid && nextBid == redeemerBid
    --           _ -> traceError "auctionConsistentDatum (checkHighestBid): expected auction state"
    --    in info'id newNodeInfo == info'id nInfo'
    --         && info'share newNodeInfo == info'share nInfo'
    --         && info'author newNodeInfo == info'author nInfo'
    --         && info'owner newNodeInfo == info'owner nInfo'
    --         && info'price newNodeInfo == info'price nInfo'
    --         && checkAuctionState
    --         && checkHighestBid

    -- Check if changed only owner and price
    consistentDatumBuy node =
      let validAuthor = info'author . node'information $ node
          validPrice = info'price . node'information $ node
          validInfo = (node'information oldNode) {info'author' = toSpooky validAuthor, info'price' = toSpooky validPrice}
          validNode = oldNode {node'information' = toSpooky validInfo}
       in validNode == node

    -- Check if nft is for sale (price is not Nothing)
    nftForSale = isJust . info'price . node'information $ oldNode

    -- Check if author of NFT receives share
    correctPaymentAuthor node = correctPayment node (info'author . node'information) calculateAuthorShare

    -- Check if owner of NFT receives share
    correctPaymentOwner node = correctPayment node (info'owner . node'information) calculateOwnerShare

    -- Check if author of NFT receives share when is also owner
    correctPaymentOnlyAuthor node = correctPayment node (info'owner . node'information) (\v _ -> lovelaceValueOf v)

    -- Check if buy bid is higher or equal than price
    bidHighEnough !bid = (fromJust . info'price . node'information $ oldNode) <= bid

    -- Check if the datum attached is also present in the set price transaction.
    correctDatumSetPrice node = (== (info'id . nInfo) node) . info'id . node'information $ oldNode

    -- Check if only thing changed in nodes is price
    consistentDatumSetPrice node =
      let validPrice = info'price . node'information $ node
          validInfo = (node'information oldNode) {info'price' = toSpooky validPrice}
          validNode = oldNode {node'information' = toSpooky validInfo}
       in validNode == node

    -- checkIsNotOnAuction = isNothing . info'auctionState . node'information

    -- Check if the price of NFT is changed by the owner of NFT
    signedByOwner node =
      case txInfoSignatories $ scriptContextTxInfo ctx of
        [pkh] -> pkh == unPaymentPubKeyHash (getUserId (info'owner $ node'information node))
        _ -> False

    -- Check If No new token is minted.
    !noMint = all ((nftCurr /=) . fst3) . flattenValue $ minted
      where
        minted = txInfoMint . scriptContextTxInfo $ ctx

    -- Check if exactly two Datums are attached to Mint output, and ids matches
    checkMissMatchDatumMint = case getOutputDatumsWithTx @DatumNft ctx of
      [x, y] -> case sort2On fst (x, y) of
        ((HeadDatum _, _), (NodeDatum datum2, tx2)) -> checkTxDatumMatch datum2 tx2
        ((NodeDatum datum1, tx1), (NodeDatum datum2, tx2)) ->
          checkTxDatumMatch datum1 tx1 && checkTxDatumMatch datum2 tx2
        _ -> False
      _ -> False

    -- Check if exactly one Node is attached to outputs, and ids matches
    checkMissMatchDatum = case filter (isNode . fst) . getOutputDatumsWithTx @DatumNft $ ctx of
      [(NodeDatum datum, tx)] -> checkTxDatumMatch datum tx
      _ -> False

    -- Check if exactly one Node is attached to inputs, and ids matches
    onlyOneNodeAttached = case filter (isNode . fst) . getInputDatumsWithTx @DatumNft $ ctx of
      [] -> traceError' "onlyOneNodeAttached: None provided"
      [(NodeDatum datum, tx)] -> checkTxDatumMatch datum tx
      _ -> traceError' "onlyOneNodeAttached: More provided"

    isNode (NodeDatum _) = True
    isNode _ = False

    -- Check if all tokens from input and mint are returned
    checkTokenReturned =
      let addr = appInstance'Address . node'appInstance $ oldNode
          inNfts =
            extractCurr nftCurr
              . (\tx -> mconcat (txOutValue . txInInfoResolved <$> txInfoInputs tx) <> txInfoMint tx)
              . scriptContextTxInfo
              $ ctx
          outNfts =
            extractCurr nftCurr
              . mconcat
              . fmap txOutValue
              . filter ((addr ==) . txOutAddress)
              . txInfoOutputs
              . scriptContextTxInfo
              $ ctx
       in inNfts == outNfts

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
txPolicy :: UniqueToken -> TypedValidator Any
txPolicy x = unsafeMkTypedValidator v
  where
    v =
      mkValidatorScript
        ($$(PlutusTx.compile [||wrap||]) `PlutusTx.applyCode` ($$(PlutusTx.compile [||mkTxPolicy||]) `PlutusTx.applyCode` PlutusTx.liftCode x))
    validatorUntyped = wrap (mkTxPolicy x)
    wrap = myWrapValidator @DatumNft @UserAct @ScriptContext

{-# INLINEABLE txValHash #-}
txValHash :: UniqueToken -> ValidatorHash
txValHash = validatorHash . txPolicy

{-# INLINEABLE txScrAddress #-}
txScrAddress :: UniqueToken -> Address
txScrAddress = toSpookyAddress . validatorAddress . txPolicy

{-# INLINEABLE curSymbol #-}

-- | Calculate the currency symbol of the NFT.
curSymbol :: NftAppInstance -> CurrencySymbol
curSymbol = toSpookyCurrencySymbol . scriptCurrencySymbol . mintPolicy

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
  assetClass
    (nftCurrency datum)
    (nftTokenName datum)

{-# INLINEABLE calculateShares #-}

{- | Returns the amount each party should be paid given the number of shares
 retained by author.
-}
calculateShares :: Integer -> Rational -> (Value, Value)
calculateShares bid authorShare = (toOwner, toAuthor)
  where
    authorPart = round $ fromInteger bid * authorShare
    toAuthor = lovelaceValueOf authorPart
    toOwner = lovelaceValueOf $ bid - authorPart

{-# INLINEABLE calculateOwnerShare #-}

-- | Returns the calculated value of shares.
calculateOwnerShare :: Integer -> Rational -> Value
calculateOwnerShare x y = fst $ calculateShares x y

{-# INLINEABLE calculateAuthorShare #-}

-- | Returns the calculated value of shares.
calculateAuthorShare :: Integer -> Rational -> Value
calculateAuthorShare x y = snd $ calculateShares x y

{-# INLINEABLE getInputDatums #-}

-- | Returns datums attached to inputs of transaction
getInputDatums :: PlutusTx.FromData a => ScriptContext -> [a]
getInputDatums = fmap fst . getInputDatumsWithTx

{-# INLINEABLE getInputDatumsWithTx #-}

-- | Returns datums and corresponding UTXOs attached to inputs of transaction
getInputDatumsWithTx :: PlutusTx.FromData a => ScriptContext -> [(a, TxOut)]
getInputDatumsWithTx ctx =
  mapMaybe (\(datum, tx) -> (,) <$> (PlutusTx.fromBuiltinData . getDatum $ datum) <*> pure tx)
    . mapMaybe (\(hash, tx) -> (,) <$> findDatum hash (scriptContextTxInfo ctx) <*> pure tx)
    . mapMaybe ((\tx -> (,) <$> txOutDatumHash tx <*> pure tx) . txInInfoResolved)
    . txInfoInputs
    . scriptContextTxInfo
    $ ctx

{-# INLINEABLE getOutputDatums #-}

-- | Returns datums attached to outputs of transaction
getOutputDatums :: PlutusTx.FromData a => ScriptContext -> [a]
getOutputDatums = fmap fst . getOutputDatumsWithTx

{-# INLINEABLE getOutputDatumsWithTx #-}

-- | Returns datums and coresponding UTXOs attached to outputs of transaction
getOutputDatumsWithTx :: PlutusTx.FromData a => ScriptContext -> [(a, TxOut)]
getOutputDatumsWithTx ctx =
  mapMaybe (\(datum, tx) -> (,) <$> (PlutusTx.fromBuiltinData . getDatum $ datum) <*> pure tx)
    . mapMaybe (\(hash, tx) -> (,) <$> findDatum hash (scriptContextTxInfo ctx) <*> pure tx)
    . mapMaybe (\tx -> (,) <$> txOutDatumHash tx <*> pure tx)
    . txInfoOutputs
    . scriptContextTxInfo
    $ ctx

-- Switch definitons for meaningful errors during development

{-# INLINEABLE traceIfFalse' #-}
traceIfFalse' :: BuiltinString -> Bool -> Bool
traceIfFalse' _ x = x

-- traceIfFalse' = traceIfFalse

{-# INLINEABLE traceError' #-}
traceError' :: BuiltinString -> a
traceError' _ = error ()

-- traceError' = traceError

{-# INLINEABLE myWrapValidator #-}
myWrapValidator ::
  forall d r p.
  (PlutusTx.UnsafeFromData d, PlutusTx.UnsafeFromData r, PlutusTx.UnsafeFromData p) =>
  (d -> r -> p -> Bool) ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
myWrapValidator f d r p = check (f (PlutusTx.unsafeFromBuiltinData d) (PlutusTx.unsafeFromBuiltinData r) (PlutusTx.unsafeFromBuiltinData p))

{-# INLINEABLE myWrapMintingPolicy #-}
myWrapMintingPolicy ::
  PlutusTx.UnsafeFromData r =>
  (r -> ScriptContext -> Bool) ->
  WrappedMintingPolicyType
-- We can use unsafeFromBuiltinData here as we would fail immediately anyway if parsing failed
myWrapMintingPolicy f r p = check $ f (PlutusTx.unsafeFromBuiltinData r) (PlutusTx.unsafeFromBuiltinData p)
