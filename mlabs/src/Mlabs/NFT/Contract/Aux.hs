module Mlabs.NFT.Contract.Aux (
  getScriptAddrUtxos,
  getUserAddr,
  getUserUtxos,
  getUId,
  getAddrUtxos,
  getAddrValidUtxos,
  serialiseDatum,
  getNftDatum,
  getsNftDatum,
  hashData,
  findNft,
  fstUtxoAt,
  entryToPointInfo,
  getDatumsTxsOrdered,
  getHead,
  getApplicationCurrencySymbol,
) where

import PlutusTx.Prelude hiding (mconcat, (<>))
import Prelude (mconcat, (<>))
import Prelude qualified as Hask

import Control.Lens (filtered, to, traversed, (^.), (^..), _Just, _Right)
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Text (Text, pack)

import Plutus.ChainIndex.Tx (ChainIndexTx)
import Plutus.Contract (Contract, utxosTxOutTxAt)
import Plutus.Contract qualified as Contract
import PlutusTx qualified

import Plutus.V1.Ledger.Value (symbols)

import Ledger (
  Address,
  ChainIndexTxOut,
  Datum (..),
  TxOutRef,
  ciTxOutDatum,
  ciTxOutValue,
  getDatum,
  pubKeyAddress,
  pubKeyHash,
  toTxOut,
  txOutValue,
 )

import Ledger.Value as Value (unAssetClass, valueOf)
import Mlabs.Plutus.Contract (readDatum')

import Mlabs.NFT.Types
import Mlabs.NFT.Validation

getScriptAddrUtxos :: Contract w s Text (Map.Map TxOutRef (ChainIndexTxOut, ChainIndexTx))
getScriptAddrUtxos = utxosTxOutTxAt txScrAddress

-- HELPER FUNCTIONS AND CONTRACTS --

-- | Get the current Wallet's publick key.
getUserAddr :: Contract w s Text Address
getUserAddr = pubKeyAddress <$> Contract.ownPubKey

-- | Get the current wallet's utxos.
getUserUtxos :: Contract w s Text (Map.Map TxOutRef Ledger.ChainIndexTxOut)
getUserUtxos = getAddrUtxos =<< getUserAddr

-- | Get the current wallet's userId.
getUId :: Contract w s Text UserId
getUId = UserId . pubKeyHash <$> Contract.ownPubKey

-- | Get the ChainIndexTxOut at an address.
getAddrUtxos :: Address -> Contract w s Text (Map.Map TxOutRef ChainIndexTxOut)
getAddrUtxos adr = Map.map fst <$> utxosTxOutTxAt adr

-- | Get the ChainIndexTxOut at an address.
getAddrValidUtxos :: NftAppSymbol -> Contract w s Text (Map.Map TxOutRef (ChainIndexTxOut, ChainIndexTx))
getAddrValidUtxos appSymbol = Map.filter validTx <$> utxosTxOutTxAt txScrAddress
  where
    validTx (cIxTxOut, _) = elem (app'symbol appSymbol) $ symbols (cIxTxOut ^. ciTxOutValue)

-- | Serialise Datum
serialiseDatum :: PlutusTx.ToData a => a -> Datum
serialiseDatum = Datum . PlutusTx.toBuiltinData

-- | Returns the Datum of a specific nftId from the Script address.
getNftDatum :: NftId -> NftAppSymbol -> Contract w s Text (Maybe DatumNft)
getNftDatum nftId appSymbol = do
  utxos :: [Ledger.ChainIndexTxOut] <- fmap fst . Map.elems <$> getAddrValidUtxos appSymbol
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
getsNftDatum :: (DatumNft -> b) -> NftId -> NftAppSymbol -> Contract a s Text (Maybe b)
getsNftDatum f nftId = fmap (fmap f) . getNftDatum nftId

-- | Find NFTs at a specific Address. Will throw an error if none or many are found.
findNft :: NftId -> NftAppSymbol -> GenericContract PointInfo
findNft nftId cSymbol = do
  utxos <- getAddrValidUtxos cSymbol
  case findData utxos of
    [v] -> do
      Contract.logInfo @Hask.String $ Hask.show $ "NFT Found:" <> Hask.show v
      pure $ pointInfo v
    [] -> Contract.throwError $ "DatumNft not found for " <> (pack . Hask.show) nftId
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
      let (cs, tn) = unAssetClass $ nftAsset datum
       in tn == nftTokenName datum -- sanity check
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

-- | Get the Head of the List
getHead :: NftAppSymbol -> GenericContract (Maybe PointInfo)
getHead aSym = do
  headX <- filter (isHead . pi'datum) <$> getDatumsTxsOrdered aSym
  case headX of
    [] -> pure Nothing
    [x] -> pure $ Just x
    _ -> do
      utxos <- getDatumsTxsOrdered aSym
      Contract.throwError $
        mconcat
          [ "This should have not happened! More than one Head Datums. Datums are: "
          , pack . Hask.show . fmap pi'datum $ utxos
          ]
  where
    isHead = \case
      HeadDatum _ -> True
      NodeDatum _ -> False

entryToPointInfo :: (TxOutRef, (ChainIndexTxOut, ChainIndexTx)) -> GenericContract PointInfo
entryToPointInfo (oref, (out, tx)) = case readDatum' @DatumNft out of
  Nothing -> Contract.throwError "Datum not found"
  Just d -> pure $ PointInfo d oref out tx

{- | Get `DatumNft` together with`TxOutRef` and `ChainIndexTxOut`
 for particular `NftAppSymbol` and return them sorted by `DatumNft`'s `Pointer`:
 head node first, list nodes ordered by pointer
-}
getDatumsTxsOrdered :: NftAppSymbol -> Contract w s Text [PointInfo]
getDatumsTxsOrdered nftAS = do
  utxos <- Map.toList <$> getAddrValidUtxos nftAS
  datums <- mapM withDatum utxos
  let sortedDatums = L.sort datums
  return sortedDatums
  where
    withDatum :: (TxOutRef, (ChainIndexTxOut, ChainIndexTx)) -> Contract w s Text PointInfo
    withDatum (oref, (out, tx)) = case readDatum' @DatumNft out of
      Nothing -> Contract.throwError "Datum not found"
      Just d -> pure $ PointInfo d oref out tx

-- | A hashing function to minimise the data to be attached to the NTFid.
hashData :: Content -> BuiltinByteString
hashData (Content b) = sha2_256 b

getApplicationCurrencySymbol :: NftAppInstance -> GenericContract NftAppSymbol
getApplicationCurrencySymbol appInstance = do
  utxos <- Contract.utxosAt . appInstance'Address $ appInstance
  let outs = fmap toTxOut . Map.elems $ utxos
      (uniqueCurrency, uniqueToken) = unAssetClass . appInstance'AppAssetClass $ appInstance
      lstHead' = find (\tx -> valueOf (txOutValue tx) uniqueCurrency uniqueToken == 1) outs
  headUtxo <- case lstHead' of
    Nothing -> Contract.throwError "Head not found"
    Just lstHead -> pure lstHead
  let currencies = filter (uniqueCurrency /=) $ symbols . txOutValue $ headUtxo
  case currencies of
    [appSymbol] -> pure . NftAppSymbol $ appSymbol
    [] -> Contract.throwError "Head does not contain AppSymbol"
    _ -> Contract.throwError "Head contains more than 2 currencies (Unreachable?)"
