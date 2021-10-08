module Business.Datum where

import Prelude
import Data.Array (catMaybes, foldM, snoc)
import Data.Bifunctor (bimap)
import Data.BigInteger (BigInteger, fromInt)
import Data.Either (Either(..), either)
import Data.Foldable (foldr)
import Data.Json.JsonTuple (JsonTuple(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), snd)
import Plutus.Contract.StateMachine.ThreadToken (ThreadToken)
import Plutus.Contracts.NftMarketplace.OffChain.ID (UserItemId(..))
import Plutus.Contracts.NftMarketplace.OnChain.Core.NFT (Bundle(..))
import Plutus.Contracts.Services.Auction.Core (Auction(..))
import Plutus.Contracts.NftMarketplace.OnChain.Core.StateMachine (MarketplaceDatum)
import Plutus.Contracts.Services.Sale.Core (Sale)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Value (CurrencySymbol, TokenName(..), Value)
import PlutusTx.AssocMap as AssocMap
import Utils.ByteString as Utils

type Item
  = Either NftSingleton NftBundle

getItemId :: Item -> UserItemId
getItemId = either (UserNftId <<< _.ipfsCid) (UserBundleId <<< map _.ipfsCid <<< _.tokens)

type ItemLot
  = Either NftSingletonLot NftBundleLot

getLot :: ItemLot -> Either Sale Auction
getLot = either _.lot _.lot

getItem :: ItemLot -> Item
getItem = bimap _.nft _.bundle

type NftSingleton
  = { ipfsCid :: String
    , currency :: CurrencySymbol
    , name :: String
    , description :: String
    , category :: Array String
    , issuer :: Maybe PubKeyHash
    }

type NftSingletonLot
  = { nft :: NftSingleton
    , lot :: Either Sale Auction
    }

type NftBundle
  = { name :: String
    , description :: String
    , category :: Array String
    , tokens :: Array NftSingleton
    }

type NftBundleLot
  = { bundle :: NftBundle
    , lot :: Either Sale Auction
    }

findNftSingletons :: Value -> MarketplaceDatum -> Array NftSingleton
findNftSingletons funds store = foldr getSingleton [] userSingletons
  where
  userSingletons :: Array (Tuple CurrencySymbol String)
  userSingletons = foldr getNft [] $ AssocMap.toTuples $ _.getValue $ unwrap funds

  getNft (Tuple currencySymbol val) acc = case AssocMap.toTuples val of
    [ Tuple (TokenName ipfsCid) quantity ]
      | quantity == fromInt 1 -> acc `snoc` Tuple currencySymbol ipfsCid.unTokenName
    _ -> acc

  marketplaceSingletons ::
    Map.Map CurrencySymbol
      { niCurrency :: CurrencySymbol
      , niName :: String
      , niDescription :: String
      , niCategory :: Array String
      , niIssuer :: Maybe PubKeyHash
      }
  marketplaceSingletons =
    Map.fromFoldable $ map (getInfo <<< unwrap <<< _.nftRecord <<< unwrap <<< snd)
      $ AssocMap.toTuples
      $ (unwrap store).mdSingletons

  getInfo record = Tuple record.niCurrency record

  getSingleton (Tuple currencySymbol ipfsCid) acc = case Map.lookup currencySymbol marketplaceSingletons of
    Nothing -> acc
    Just record ->
      acc
        `snoc`
          { ipfsCid: ipfsCid
          , currency: currencySymbol
          , name: Utils.decodeUtf8 record.niName
          , description: Utils.decodeUtf8 record.niDescription
          , category: map Utils.decodeUtf8 record.niCategory
          , issuer: record.niIssuer
          }

findNftBundles :: Value -> MarketplaceDatum -> Array NftBundle
findNftBundles funds store = foldr getBundle [] marketplaceBundles
  where
  userSingletons :: Map.Map CurrencySymbol String
  userSingletons = Map.fromFoldable $ foldr getNft [] $ AssocMap.toTuples $ _.getValue $ unwrap funds

  getNft (Tuple currencySymbol val) acc = case AssocMap.toTuples val of
    [ Tuple (TokenName ipfsCid) quantity ]
      | quantity == fromInt 1 -> acc `snoc` Tuple currencySymbol ipfsCid.unTokenName
    _ -> acc

  marketplaceBundles ::
    Array
      { bundleInfo ::
          { biCategory :: Array String
          , biDescription :: String
          , biName :: String
          }
      , tokens ::
          Array
            { niCategory :: Array String
            , niCurrency :: CurrencySymbol
            , niDescription :: String
            , niIssuer :: Maybe PubKeyHash
            , niName :: String
            }
      }
  marketplaceBundles =
    catMaybes
      $ map (getInfo <<< unwrap <<< snd)
      $ AssocMap.toTuples
      $ (unwrap store).mdBundles

  getInfo record = case record.nbTokens of
    NoLot tokens ->
      Just
        { bundleInfo: unwrap record.nbRecord
        , tokens: map (unwrap <<< snd) $ AssocMap.toTuples tokens
        }
    _ -> Nothing

  getBundle bundle acc = case foldM lookupToken [] bundle.tokens of
    Nothing -> acc
    Just ts ->
      acc
        `snoc`
          { name: Utils.decodeUtf8 bundle.bundleInfo.biName
          , description: Utils.decodeUtf8 bundle.bundleInfo.biDescription
          , category: Utils.decodeUtf8 <$> bundle.bundleInfo.biCategory
          , tokens: ts
          }

  lookupToken acc nft = case Map.lookup nft.niCurrency userSingletons of
    Nothing -> Nothing
    Just ipfsCid ->
      Just
        $ acc
            `snoc`
              { ipfsCid: ipfsCid
              , currency: nft.niCurrency
              , name: Utils.decodeUtf8 nft.niName
              , description: Utils.decodeUtf8 nft.niDescription
              , category: map Utils.decodeUtf8 nft.niCategory
              , issuer: nft.niIssuer
              }

findNftSingletonLots :: MarketplaceDatum -> Array NftSingletonLot
findNftSingletonLots store = map getSingleton marketplaceSingletons
  where
  marketplaceSingletons ::
    Array
      ( Tuple (Tuple String (Either Sale Auction))
          { niCurrency :: CurrencySymbol
          , niName :: String
          , niDescription :: String
          , niCategory :: Array String
          , niIssuer :: Maybe PubKeyHash
          }
      )
  marketplaceSingletons =
    catMaybes
      $ map (getInfo <<< unwrap <<< snd)
      $ AssocMap.toTuples
      $ (unwrap store).mdSingletons

  getInfo record = case record.nftLot of
    Just lot -> Just $ Tuple (unwrap lot) (unwrap record.nftRecord)
    Nothing -> Nothing

  getSingleton (Tuple (Tuple ipfsCid lot) record) =
    { nft:
        { ipfsCid: Utils.decodeUtf8 ipfsCid
        , currency: record.niCurrency
        , name: Utils.decodeUtf8 record.niName
        , description: Utils.decodeUtf8 record.niDescription
        , category: map Utils.decodeUtf8 record.niCategory
        , issuer: record.niIssuer
        }
    , lot:
        case lot of
          Left sale -> Left sale
          Right auction -> Right auction
    }

findNftBundleLots :: MarketplaceDatum -> Array NftBundleLot
findNftBundleLots store =
  catMaybes
    $ map (getInfo <<< unwrap <<< snd)
    $ AssocMap.toTuples
    $ (unwrap store).mdBundles
  where
  getInfo record = case record.nbTokens of
    HasLot tokens lot ->
      let
        bundleInfo = unwrap record.nbRecord
      in
        Just
          { bundle:
              { name: Utils.decodeUtf8 bundleInfo.biName
              , description: Utils.decodeUtf8 bundleInfo.biDescription
              , category: Utils.decodeUtf8 <$> bundleInfo.biCategory
              , tokens: map (getToken <<< unwrap <<< snd) $ AssocMap.toTuples tokens
              }
          , lot:
              case lot of
                Left sale -> Left sale
                Right auction -> Right auction
          }
    _ -> Nothing

  getToken (Tuple ipfsCid record) =
    let
      nft = unwrap record
    in
      { ipfsCid: Utils.decodeUtf8 ipfsCid
      , currency: nft.niCurrency
      , name: Utils.decodeUtf8 nft.niName
      , description: Utils.decodeUtf8 nft.niDescription
      , category: map Utils.decodeUtf8 nft.niCategory
      , issuer: nft.niIssuer
      }
