module Mlabs.NFT.Contract.Aux (
  entryToPointInfo,
  findNft,
  fstUtxoAt,
  getAddrUtxos,
  getAddrValidUtxos,
  getApplicationCurrencySymbol,
  getDatumsTxsOrdered,
  getDatumsTxsOrderedFromAddr,
  getGovHead,
  getNftAppSymbol,
  getNftDatum,
  getNftHead,
  getScriptAddrUtxos,
  getsNftDatum,
  getUId,
  getUserAddr,
  getUserUtxos,
  hashData,
  serialiseDatum,
  toDatum,
) where

import PlutusTx qualified
import PlutusTx.Prelude hiding (mconcat, (<>))
import Prelude (mconcat, (<>))
import Prelude qualified as Hask

import Control.Lens (filtered, to, traversed, (^.), (^..), _Just, _Right)
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Text (Text, pack)

import Plutus.ChainIndex.Tx (ChainIndexTx)
import Plutus.Contract (utxosTxOutTxAt)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Value (assetClassValueOf, symbols)

import Ledger (
  Address,
  ChainIndexTxOut,
  Datum (..),
  TxOutRef,
  ciTxOutDatum,
  ciTxOutValue,
  getDatum,
  pubKeyHashAddress,
  toTxOut,
  txOutValue,
 )
import Ledger.Value as Value (unAssetClass, valueOf)

import Mlabs.NFT.Governance.Types (GovDatum (gov'list), LList (HeadLList))
import Mlabs.NFT.Spooky (toSpooky, toSpookyCurrencySymbol, unSpookyAddress, unSpookyAssetClass, unSpookyTokenName)
import Mlabs.NFT.Types (
  Content,
  DatumNft (..),
  GenericContract,
  NftAppInstance,
  NftAppSymbol (NftAppSymbol),
  NftId,
  NftListHead,
  PointInfo (PointInfo, pi'CITxO, pi'data),
  UniqueToken,
  UserId (UserId),
  app'symbol,
  appInstance'Address,
  appInstance'UniqueToken,
  getContent,
  info'id,
  nftTokenName,
  node'information,
 )
import Mlabs.NFT.Validation (nftAsset, txScrAddress)
import Mlabs.Plutus.Contract (readDatum')

getScriptAddrUtxos ::
  UniqueToken ->
  GenericContract (Map.Map TxOutRef (ChainIndexTxOut, ChainIndexTx))
getScriptAddrUtxos = utxosTxOutTxAt . unSpookyAddress . txScrAddress

-- HELPER FUNCTIONS AND CONTRACTS --

-- | Convert to Datum
toDatum :: PlutusTx.ToData a => a -> Datum
toDatum = Datum . PlutusTx.toBuiltinData

-- | Get the current Wallet's publick key.
getUserAddr :: GenericContract Address
getUserAddr = (`pubKeyHashAddress` Nothing) <$> Contract.ownPaymentPubKeyHash

-- | Get the current wallet's utxos.
getUserUtxos :: GenericContract (Map.Map TxOutRef Ledger.ChainIndexTxOut)
getUserUtxos = getAddrUtxos =<< getUserAddr

-- | Get the current wallet's userId.
getUId :: GenericContract UserId
getUId = UserId . toSpooky <$> Contract.ownPaymentPubKeyHash

-- | Get the ChainIndexTxOut at an address.
getAddrUtxos :: Address -> GenericContract (Map.Map TxOutRef ChainIndexTxOut)
getAddrUtxos adr = Map.map fst <$> utxosTxOutTxAt adr

{- | Get the Head of the List, by filtering away all the utxos that don't
 contain the unique token.
-}
getHead :: UniqueToken -> GenericContract (Maybe (PointInfo NftListHead))
getHead uT = do
  utxos <- utxosTxOutTxAt . unSpookyAddress . txScrAddress $ uT
  let headUtxos = Map.toList . Map.filter containUniqueToken $ utxos
  case headUtxos of
    [] -> pure Nothing
    [(oRef, (xOut, x))] -> do
      case readDatum' @DatumNft xOut of
        Just (HeadDatum datum) ->
          pure . Just $ PointInfo datum oRef xOut x
        _ -> Contract.throwError "Head has corrupted datum!"
    _ -> do
      Contract.throwError $
        mconcat
          [ "This should have not happened! More than one Heads with Unique Tokens."
          --, pack . Hask.show . fmap pi'data $ utxos
          ]
  where
    containUniqueToken = (/= 0) . flip assetClassValueOf (unSpookyAssetClass uT) . (^. ciTxOutValue) . fst

-- | Get the  Symbol
getNftAppSymbol :: UniqueToken -> GenericContract NftAppSymbol
getNftAppSymbol uT = do
  lHead <- getHead uT
  case lHead of
    Nothing -> err
    Just headInfo -> do
      let uTCS = fst . unAssetClass . unSpookyAssetClass $ uT
      let val = filter (\x -> x /= uTCS && x /= "") . symbols $ pi'CITxO headInfo ^. ciTxOutValue
      case val of
        [x] -> pure . NftAppSymbol . toSpooky . toSpookyCurrencySymbol $ x
        [] -> Contract.throwError "Could not establish App Symbol. Does it exist in the HEAD?"
        _ -> Contract.throwError "Could not establish App Symbol. Too many symbols to distinguish from."
  where
    err = Contract.throwError "Could not establish App Symbol."

-- | Get the ChainIndexTxOut at an address.
getAddrValidUtxos :: UniqueToken -> GenericContract (Map.Map TxOutRef (ChainIndexTxOut, ChainIndexTx))
getAddrValidUtxos ut = do
  appSymbol <- getNftAppSymbol ut
  Map.filter (validTx appSymbol) <$> utxosTxOutTxAt (unSpookyAddress . txScrAddress $ ut)
  where
    validTx appSymbol (cIxTxOut, _) = elem (app'symbol appSymbol) (fmap toSpookyCurrencySymbol (symbols (cIxTxOut ^. ciTxOutValue)))

-- | Serialise Datum
serialiseDatum :: PlutusTx.ToData a => a -> Datum
serialiseDatum = Datum . PlutusTx.toBuiltinData

-- | Returns the Datum of a specific nftId from the Script address.
getNftDatum :: NftId -> UniqueToken -> GenericContract (Maybe DatumNft)
getNftDatum nftId ut = do
  utxos :: [Ledger.ChainIndexTxOut] <- fmap fst . Map.elems <$> getAddrValidUtxos ut
  let datums :: [DatumNft] =
        utxos
          ^.. traversed . Ledger.ciTxOutDatum
            . _Right
            . to (PlutusTx.fromBuiltinData @DatumNft . getDatum)
            . _Just
            . filtered
              ( \case
                  HeadDatum _ -> False
                  NodeDatum node ->
                    let nftId' = info'id . node'information $ node
                     in nftId' == nftId
              )
  Contract.logInfo @Hask.String $ Hask.show $ "Datum Found:" <> Hask.show datums
  Contract.logInfo @Hask.String $ Hask.show $ "Datum length:" <> Hask.show (Hask.length datums)
  case datums of
    [x] ->
      pure $ Just x
    [] -> do
      Contract.logError @Hask.String "No suitable Datum can be found."
      pure Nothing
    _ : _ -> do
      Contract.logError @Hask.String "More than one suitable Datum can be found. This should never happen."
      pure Nothing

{- | Gets the Datum of a specific nftId from the Script address, and applies an
  extraction function to it.
-}
getsNftDatum :: (DatumNft -> b) -> NftId -> UniqueToken -> GenericContract (Maybe b)
getsNftDatum f nftId = fmap (fmap f) . getNftDatum nftId

-- | Find NFTs at a specific Address. Will throw an error if none or many are found.
findNft :: NftId -> UniqueToken -> GenericContract (PointInfo DatumNft)
findNft nftId ut = do
  utxos <- getAddrValidUtxos ut
  case findData utxos of
    [v] -> do
      Contract.logInfo @Hask.String $ Hask.show $ "findNft: NFT Found:" <> Hask.show v
      pure $ pointInfo v
    [] -> Contract.throwError $ "findNft: DatumNft not found for " <> (pack . Hask.show) nftId
    _ ->
      Contract.throwError $
        "Should not happen! More than one DatumNft found for "
          <> (pack . Hask.show) nftId
  where
    findData =
      L.filter hasCorrectNft -- filter only datums with desired NftId
        . mapMaybe readTxData -- map to Maybe (TxOutRef, ChainIndexTxOut, DatumNft)
        . Map.toList

    readTxData (oref, (ciTxOut, ciTx)) = (oref,ciTxOut,,ciTx) <$> readDatum' ciTxOut

    hasCorrectNft (_, ciTxOut, datum, _) =
      let (cs, tn) = unAssetClass . unSpookyAssetClass $ nftAsset datum
       in tn == (unSpookyTokenName . nftTokenName $ datum) -- sanity check
            && case datum of
              NodeDatum datum' ->
                (info'id . node'information $ datum') == nftId -- check that Datum has correct NftId
                  && valueOf (ciTxOut ^. ciTxOutValue) cs tn == 1 -- check that UTXO has single NFT in Value
              HeadDatum _ -> False
    pointInfo (oR, cIxO, d, cIx) = PointInfo d oR cIxO cIx

-- | Get first utxo at address. Will throw an error if no utxo can be found.
fstUtxoAt :: Address -> GenericContract (TxOutRef, ChainIndexTxOut)
fstUtxoAt address = do
  utxos <- Contract.utxosAt address
  case Map.toList utxos of
    [] -> Contract.throwError @Text "No utxo found at address."
    x : _ -> pure x

-- | Get the Head of the NFT List
getNftHead :: UniqueToken -> GenericContract (Maybe (PointInfo DatumNft))
getNftHead ut = do
  headX <- filter (isHead . pi'data) <$> getDatumsTxsOrdered ut
  case headX of
    [] -> pure Nothing
    [x] -> pure $ Just x
    _ -> do
      utxos <- getDatumsTxsOrdered @DatumNft ut
      Contract.throwError $
        mconcat
          [ "This should have not happened! More than one Head Datums. Datums are: "
          , pack . Hask.show . fmap pi'data $ utxos
          ]
  where
    isHead = \case
      HeadDatum _ -> True
      NodeDatum _ -> False

-- | Get the Head of the Gov List
getGovHead :: Address -> GenericContract (Maybe (PointInfo GovDatum))
getGovHead addr = do
  headX <- filter f <$> getDatumsTxsOrderedFromAddr @GovDatum addr
  case headX of
    [] -> pure Nothing
    [x] -> pure $ Just x
    _ -> do
      utxos <- getDatumsTxsOrderedFromAddr @GovDatum addr
      Contract.throwError $
        mconcat
          [ "This should have not happened! More than one Head Datums. Datums are: "
          , pack . Hask.show . fmap pi'data $ utxos
          ]
  where
    f = isHead . gov'list . pi'data

    isHead = \case
      HeadLList {} -> True
      _ -> False

entryToPointInfo ::
  (PlutusTx.FromData a) =>
  (TxOutRef, (ChainIndexTxOut, ChainIndexTx)) ->
  GenericContract (PointInfo a)
entryToPointInfo (oref, (out, tx)) = case readDatum' out of
  Nothing -> Contract.throwError "entryToPointInfo: Datum not found"
  Just d -> pure $ PointInfo d oref out tx

{- | Get `DatumNft` together with`TxOutRef` and `ChainIndexTxOut`
 for particular `NftAppSymbol` and return them sorted by `DatumNft`'s `Pointer`:
 head node first, list nodes ordered by pointer
-}
getDatumsTxsOrdered ::
  forall a.
  (PlutusTx.FromData a, Ord a, Hask.Eq a) =>
  UniqueToken ->
  GenericContract [PointInfo a]
getDatumsTxsOrdered ut = do
  utxos <- Map.toList <$> getAddrValidUtxos ut
  let datums = mapMaybe toPointInfo utxos
  let sortedDatums = L.sort datums
  case sortedDatums of
    [] -> Contract.throwError "getDatumsTxsOrdered: Datum not found"
    ds -> return ds
  where
    toPointInfo (oref, (out, tx)) = case readDatum' @a out of
      Nothing -> Nothing
      Just d -> pure $ PointInfo d oref out tx

getDatumsTxsOrderedFromAddr ::
  forall a.
  (PlutusTx.FromData a, Ord a, Hask.Eq a) =>
  Address ->
  GenericContract [PointInfo a]
getDatumsTxsOrderedFromAddr addr = do
  utxos <- Map.toList <$> utxosTxOutTxAt addr
  let datums = mapMaybe toPointInfo utxos
  let sortedDatums = L.sort datums
  case sortedDatums of
    [] -> Contract.throwError "getDatumsTxsOrderedFromAddr: Datum not found"
    ds -> return ds
  where
    toPointInfo (oref, (out, tx)) = case readDatum' @a out of
      Nothing -> Nothing
      Just d -> pure $ PointInfo d oref out tx

-- | A hashing function to minimise the data to be attached to the NTFid.
hashData :: Content -> BuiltinByteString
hashData = sha2_256 . getContent

getApplicationCurrencySymbol :: NftAppInstance -> GenericContract NftAppSymbol
getApplicationCurrencySymbol appInstance = do
  utxos <- Contract.utxosAt . unSpookyAddress . appInstance'Address $ appInstance
  let outs = fmap toTxOut . Map.elems $ utxos
      (uniqueCurrency, uniqueToken) = unAssetClass . unSpookyAssetClass . appInstance'UniqueToken $ appInstance
      lstHead' = find (\tx -> valueOf (Ledger.txOutValue tx) uniqueCurrency uniqueToken == 1) outs
  headUtxo <- case lstHead' of
    Nothing -> Contract.throwError "Head not found"
    Just lstHead -> pure lstHead
  let currencies = filter (uniqueCurrency /=) $ symbols . Ledger.txOutValue $ headUtxo
  case currencies of
    [appSymbol] -> pure . NftAppSymbol . toSpooky . toSpookyCurrencySymbol $ appSymbol
    [] -> Contract.throwError "Head does not contain AppSymbol"
    _ -> Contract.throwError "Head contains more than 2 currencies (Unreachable?)"
