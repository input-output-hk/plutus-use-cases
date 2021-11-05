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
  TxOut (..),
  ValidatorHash,
  Value,
  findDatum,
  mkMintingPolicyScript,
  ownCurrencySymbol,
  scriptContextTxInfo,
  scriptCurrencySymbol,
  txInInfoResolved,
  txInfoInputs,
  txInfoMint,
  txInfoOutputs,
  txInfoSignatories,
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
  valueOf,
 )
import Plutus.V1.Ledger.Value (AssetClass (..), assetClassValueOf, isZero)
import PlutusTx qualified

import Data.Maybe (catMaybes)
import Mlabs.NFT.Types (
  DatumNft (..),
  InformationNft (
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
        && traceIfFalse "Excatly one NFT must be minted" (checkMintedAmount nftid)
        && traceIfFalse "Old first node must point to second node." (first `pointsTo'` second)
        && traceIfFalse "New first node must point to new node." (newFirst `pointsTo` newInserted)
        && traceIfFalse "New node must point to second node." (newInserted `pointsTo'` second)
        && traceIfFalse "New node must be smaller than second node." newIsSmallerThanSecond
        && traceIfFalse "New price cannot be negative." priceNotNegative'
        && traceIfFalse "Currency symbol must match app instance" checkCurrencySymbol
        && traceIfFalse "Minted token must be sent to script address" (checkSentAddress nftid)
        && traceIfFalse "Nodes must be sent to script address" checkNodesAddresses
    Initialise ->
      traceIfFalse "The token is not present." True -- todo
        && traceIfFalse "Only One Unique Token Can be Minted" True -- todo
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
          MintAct {} -> True
          BuyAct {..} ->
            traceIfFalse "Transaction cannot mint." noMint
              && traceIfFalse "NFT not for sale." nftForSale
              && traceIfFalse "New Price cannot be negative." (priceNotNegative act'newPrice)
              && traceIfFalse "Act'Bid is too low for the NFT price." (bidHighEnough act'bid)
              && traceIfFalse "Datum is not consistent, illegaly altered." consistentDatumBuy
              && if ownerIsAuthor
                then traceIfFalse "Amount paid to author/owner does not match act'bid." (correctPaymentOnlyAuthor act'bid)
                else
                  traceIfFalse "Current owner is not paid their share." (correctPaymentOwner act'bid)
                    && traceIfFalse "Author is not paid their share." (correctPaymentAuthor act'bid)
          SetPriceAct {..} ->
            traceIfFalse "Transaction cannot mint." noMint
              && traceIfFalse "Datum does not correspond to NFTId, no datum is present, or more than one suitable datums are present." correctDatumSetPrice
              && traceIfFalse "New Price cannot be negative." (priceNotNegative act'newPrice)
              && traceIfFalse "Only owner exclusively can set NFT price." ownerSetsPrice
              && traceIfFalse "Datum is not consistent, illegaly altered." consistentDatumSetPrice
      where
        !nInfo = node'information node
        oldDatum :: DatumNft = head . getInputDatums $ ctx

        oldNode :: NftListNode = case getNode oldDatum of
          Just n -> n
          Nothing -> traceError "Input datum is Head."

        ------------------------------------------------------------------------------
        -- Utility functions.

        containsNft !v = valueOf v (app'symbol . act'symbol $ act) (nftTokenName datum') == 1

        !getAda = flip assetClassValueOf $ assetClass Ada.adaSymbol Ada.adaToken

        -- Check if the Person is being reimbursed accordingly, with the help of 2
        -- getter functions. Helper function.
        correctPayment !userIdGetter !shareCalcFn !bid = personGetsAda >= personWantsAda
          where
            info = scriptContextTxInfo ctx
            personId = getUserId . userIdGetter $ node
            share = info'share . node'information $ node
            personGetsAda = getAda $ valuePaidTo info personId
            personWantsAda = getAda $ shareCalcFn bid share

        !ownerIsAuthor =
          (info'owner . node'information $ oldNode) == (info'author . node'information $ oldNode)

        getNode = \case
          NodeDatum n -> Just n
          _ -> Nothing

        ------------------------------------------------------------------------------
        -- Checks

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
            info = scriptContextTxInfo ctx
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
        !ownerSetsPrice =
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
