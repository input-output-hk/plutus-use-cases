module Mlabs.NFT.Contract (
  NFTAppSchema,
  schemas,
  endpoints,
  queryEndpoints,
) where

import PlutusTx.Prelude hiding (mconcat, (<>))
import Prelude (mconcat, (<>))
import Prelude qualified as Hask

import Control.Lens (filtered, to, traversed, (^.), (^..), _Just, _Right)
import Control.Monad (join, void)
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.Text (Text, pack)

import Text.Printf (printf)

import Plutus.Contract (Contract, Endpoint, endpoint, utxosTxOutTxAt, type (.\/))
import Plutus.Contract qualified as Contract
import PlutusTx qualified

import Ledger (
  Address,
  ChainIndexTxOut,
  Datum (..),
  Redeemer (..),
  TxOutRef,
  ciTxOutDatum,
  ciTxOutValue,
  getDatum,
  pubKeyAddress,
  pubKeyHash,
  scriptCurrencySymbol,
  txId,
 )

import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (validatorScript)
import Ledger.Value as Value (TokenName (..), singleton, unAssetClass, valueOf)

import Playground.Contract (mkSchemaDefinitions)

import Mlabs.NFT.Types (
  BuyRequestUser (..),
  Content (..),
  MintParams (..),
  NftId (..),
  QueryResponse (..),
  SetPriceParams (..),
  UserId (..),
 )

import Mlabs.NFT.Validation (
  DatumNft (..),
  NftTrade,
  UserAct (..),
  asRedeemer,
  calculateShares,
  mintPolicy,
  nftAsset,
  nftCurrency,
  priceNotNegative,
  txPolicy,
  txScrAddress,
 )

import Mlabs.Plutus.Contract (readDatum', selectForever)

-- | A contract used exclusively for query actions.
type QueryContract a = Contract (Last QueryResponse) NFTAppSchema Text a

-- | A contract used for all user actions.
type UserContract a = Contract (Last NftId) NFTAppSchema Text a

-- | A common App schema works for now.
type NFTAppSchema =
  -- Author Endpoint
  Endpoint "mint" MintParams
    -- User Action Endpoints
    .\/ Endpoint "buy" BuyRequestUser
    .\/ Endpoint "set-price" SetPriceParams
    -- Query Endpoints
    .\/ Endpoint "query-current-owner" NftId
    .\/ Endpoint "query-current-price" NftId

mkSchemaDefinitions ''NFTAppSchema

-- MINT --

-- | Mints an NFT and sends it to the App Address.
mint :: MintParams -> UserContract ()
mint nftContent = do
  addr <- getUserAddr
  nft <- nftInit nftContent
  utxos <- Contract.utxosAt addr
  oref <- fstUtxo addr
  let nftId = dNft'id nft
      scrAddress = txScrAddress
      nftPolicy = mintPolicy scrAddress oref nftId
      val = Value.singleton (scriptCurrencySymbol nftPolicy) (nftId'token nftId) 1
      (lookups, tx) =
        ( mconcat
            [ Constraints.unspentOutputs utxos
            , Constraints.mintingPolicy nftPolicy
            , Constraints.typedValidatorLookups txPolicy
            ]
        , mconcat
            [ Constraints.mustMintValue val
            , Constraints.mustSpendPubKeyOutput oref
            , Constraints.mustPayToTheScript nft val
            ]
        )
  void $ Contract.submitTxConstraintsWith @NftTrade lookups tx
  Contract.tell . Last . Just $ nftId
  Contract.logInfo @Hask.String $ printf "forged %s" (Hask.show val)

-- | Initialise an NFT using the current wallet.
nftInit :: MintParams -> Contract w s Text DatumNft
nftInit mintP = do
  user <- getUId
  nftId <- nftIdInit mintP
  pure $
    DatumNft
      { dNft'id = nftId
      , dNft'share = mp'share mintP
      , dNft'author = user
      , dNft'owner = user
      , dNft'price = mp'price mintP
      }

-- | Initialise new NftId
nftIdInit :: MintParams -> Contract w s Text NftId
nftIdInit mP = do
  userAddress <- getUserAddr
  oref <- fstUtxo userAddress
  let hData = hashData $ mp'content mP
  pure $
    NftId
      { nftId'title = mp'title mP
      , nftId'token = TokenName hData
      , nftId'outRef = oref
      }

{- | BUY.
 Attempts to buy a new NFT by changing the owner, pays the current owner and
 the author, and sets a new price for the NFT.
-}
buy :: BuyRequestUser -> Contract w NFTAppSchema Text ()
buy (BuyRequestUser nftId bid newPrice) = do
  oldDatum' <- getNftDatum nftId
  case oldDatum' of
    Nothing -> Contract.logError @Hask.String "NFT Cannot be found."
    Just oldDatum -> do
      let scrAddress = txScrAddress
          oref = nftId'outRef . dNft'id $ oldDatum
          nftPolicy = mintPolicy scrAddress oref nftId
          val = Value.singleton (scriptCurrencySymbol nftPolicy) (nftId'token nftId) 1
      case dNft'price oldDatum of
        Nothing -> Contract.logError @Hask.String "NFT not for sale."
        Just price ->
          if bid < price
            then Contract.logError @Hask.String "Bid Price is too low."
            else do
              user <- getUId
              userUtxos <- getUserUtxos
              (nftOref, ciTxOut, _) <- findNft txScrAddress nftId
              oref' <- fstUtxo =<< getUserAddr
              let nftPolicy' = mintPolicy scrAddress oref' nftId
                  nftCurrency' = nftCurrency nftId
                  newDatum' =
                    -- Unserialised Datum
                    DatumNft
                      { dNft'id = dNft'id oldDatum
                      , dNft'share = dNft'share oldDatum
                      , dNft'author = dNft'author oldDatum
                      , dNft'owner = user
                      , dNft'price = newPrice
                      }
                  action =
                    BuyAct
                      { act'bid = bid
                      , act'newPrice = newPrice
                      , act'cs = nftCurrency'
                      }
                  newDatum = Datum . PlutusTx.toBuiltinData $ newDatum' -- Serialised Datum
                  (paidToOwner, paidToAuthor) = calculateShares bid $ dNft'share oldDatum
                  newValue = ciTxOut ^. ciTxOutValue
                  (lookups, tx) =
                    ( mconcat
                        [ Constraints.unspentOutputs userUtxos
                        , Constraints.typedValidatorLookups txPolicy
                        , Constraints.mintingPolicy nftPolicy'
                        , Constraints.otherScript (validatorScript txPolicy)
                        , Constraints.unspentOutputs $ Map.singleton nftOref ciTxOut
                        ]
                    , mconcat
                        [ Constraints.mustPayToTheScript newDatum' newValue
                        , Constraints.mustIncludeDatum newDatum
                        , Constraints.mustPayToPubKey (getUserId . dNft'owner $ oldDatum) paidToOwner
                        , Constraints.mustPayToPubKey (getUserId . dNft'author $ oldDatum) paidToAuthor
                        , Constraints.mustSpendScriptOutput
                            nftOref
                            (Redeemer . PlutusTx.toBuiltinData $ action)
                        ]
                    )
              void $ Contract.submitTxConstraintsWith @NftTrade lookups tx
              void $ Contract.logInfo @Hask.String $ printf "Bought %s" $ Hask.show val

-- SET PRICE --
setPrice :: SetPriceParams -> Contract w NFTAppSchema Text ()
setPrice spParams = do
  result <-
    Contract.runError $ do
      (oref, ciTxOut, datum) <- findNft txScrAddress $ sp'nftId spParams
      runOffChainChecks datum
      let (tx, lookups) = mkTxLookups oref ciTxOut datum
      ledgerTx <- Contract.submitTxConstraintsWith @NftTrade lookups tx
      void $ Contract.awaitTxConfirmed $ Ledger.txId ledgerTx
  either Contract.logError (const $ Contract.logInfo @Hask.String "New price set") result
  where
    mkTxLookups oref ciTxOut datum =
      let newDatum = datum {dNft'price = sp'price spParams}
          redeemer = asRedeemer $ SetPriceAct (sp'price spParams) $ nftCurrency (dNft'id datum)
          newValue = ciTxOut ^. ciTxOutValue
          lookups =
            mconcat
              [ Constraints.unspentOutputs $ Map.singleton oref ciTxOut
              , Constraints.typedValidatorLookups txPolicy
              , Constraints.otherScript (validatorScript txPolicy)
              ]
          tx =
            mconcat
              [ Constraints.mustSpendScriptOutput oref redeemer
              , Constraints.mustPayToTheScript newDatum newValue
              ]
       in (tx, lookups)

    runOffChainChecks :: DatumNft -> Contract w NFTAppSchema Text ()
    runOffChainChecks datum = do
      ownPkh <- pubKeyHash <$> Contract.ownPubKey
      if isOwner datum ownPkh
        then pure ()
        else Contract.throwError "Only owner can set price"
      if priceNotNegative (sp'price spParams)
        then pure ()
        else Contract.throwError "New price can not be negative"

    isOwner datum pkh = pkh == (getUserId . dNft'owner) datum

{- | Query the current price of a given NFTid. Writes it to the Writer instance
 and also returns it, to be used in other contracts.
-}
queryCurrentPrice :: NftId -> QueryContract QueryResponse
queryCurrentPrice nftid = do
  price <- wrap <$> getsNftDatum dNft'price nftid
  Contract.tell (Last $ pure price) >> log price >> return price
  where
    wrap = QueryCurrentPrice . Last . join
    log price =
      Contract.logInfo @Hask.String $
        "Current price of: " <> Hask.show nftid <> " is: " <> Hask.show price

{- | Query the current owner of a given NFTid. Writes it to the Writer instance
 and also returns it, to be used in other contracts.
-}
queryCurrentOwner :: NftId -> QueryContract QueryResponse
queryCurrentOwner nftid = do
  ownerResp <- wrap <$> getsNftDatum dNft'owner nftid
  Contract.tell (Last $ pure ownerResp) >> log ownerResp >> return ownerResp
  where
    wrap = QueryCurrentOwner . Last
    log owner =
      Contract.logInfo @Hask.String $
        "Current owner of: " <> Hask.show nftid <> " is: " <> Hask.show owner

-- ENDPOINTS --

-- | User Endpoints .
endpoints :: UserContract ()
endpoints =
  selectForever
    [ endpoint @"mint" mint
    , endpoint @"buy" buy
    , endpoint @"set-price" setPrice
    ]

-- Query Endpoints are used for Querying, with no on-chain tx generation.
queryEndpoints :: QueryContract ()
queryEndpoints =
  selectForever
    [ endpoint @"query-current-price" queryCurrentPrice
    , endpoint @"query-current-owner" queryCurrentOwner
    ]

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

-- | Get first utxo at address. Will throw an error if no utxo can be found.
fstUtxo :: Address -> Contract w s Text TxOutRef
fstUtxo address = do
  utxos <- Contract.utxosAt address
  case Map.keys utxos of
    [] -> Contract.throwError @Text "No utxo found at address."
    x : _ -> pure x

-- | Returns the Datum of a specific nftId from the Script address.
getNftDatum :: NftId -> Contract w s Text (Maybe DatumNft)
getNftDatum nftId = do
  utxos :: [Ledger.ChainIndexTxOut] <- Map.elems <$> getAddrUtxos txScrAddress
  let datums :: [DatumNft] =
        utxos
          ^.. traversed . Ledger.ciTxOutDatum
            . _Right
            . to (PlutusTx.fromBuiltinData @DatumNft . getDatum)
            . _Just
            . filtered (\d -> dNft'id d == nftId)
  Contract.logInfo @Hask.String $ Hask.show $ "Datum Found:" <> Hask.show datums
  Contract.logInfo @Hask.String $ Hask.show $ "Datum length:" <> Hask.show (Hask.length datums)
  case datums of
    [x] -> pure $ Just x
    [] -> Contract.throwError "No Datum can be found."
    _ : _ -> Contract.throwError "More than one suitable Datums can be found."

{- | Gets the Datum of a specific nftId from the Script address, and applies an
 extraction function to it.
-}
getsNftDatum :: (DatumNft -> b) -> NftId -> Contract a s Text (Maybe b)
getsNftDatum f = fmap (fmap f) . getNftDatum

-- | A hashing function to minimise the data to be attached to the NTFid.
hashData :: Content -> BuiltinByteString
hashData (Content b) = sha2_256 b

-- | Find NFTs at a specific Address. Will throw an error if none or many are found.
findNft :: Address -> NftId -> Contract w s Text (TxOutRef, ChainIndexTxOut, DatumNft)
findNft addr nftId = do
  utxos <- Contract.utxosTxOutTxAt addr
  case findData utxos of
    [v] -> do
      Contract.logInfo @Hask.String $ Hask.show $ "NFT Found:" <> Hask.show v
      pure v
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
    readTxData (oref, (ciTxOut, _)) = (oref,ciTxOut,) <$> readDatum' ciTxOut
    hasCorrectNft (_, ciTxOut, datum) =
      let (cs, tn) = unAssetClass $ nftAsset nftId
       in tn == nftId'token nftId -- sanity check
            && datum.dNft'id == nftId -- check that Datum has correct NftId
            && valueOf (ciTxOut ^. ciTxOutValue) cs tn == 1 -- check that UTXO has single NFT in Value
