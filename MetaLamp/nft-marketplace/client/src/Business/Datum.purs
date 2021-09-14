module Business.Datum where

import Prelude
import Data.Array (catMaybes, snoc)
import Data.BigInteger (fromInt)
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Json.JsonTuple (JsonTuple(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), snd)
import Plutus.Contract.StateMachine.ThreadToken (ThreadToken)
import Plutus.Contracts.NftMarketplace.OnChain.Core.StateMachine (MarketplaceDatum)
import Plutus.Contracts.Services.Sale.Core (Sale)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.Value (CurrencySymbol, TokenName(..), Value)
import PlutusTx.AssocMap as AssocMap
import Utils.ByteString as Utils

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

type Auction
  = { threadToken :: ThreadToken
    , owner :: PubKeyHash
    , value :: Value
    , endTime :: POSIXTime
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

findNftSingletonLots :: MarketplaceDatum -> Array NftSingletonLot
findNftSingletonLots store = map getSingleton marketplaceSingletons
  where
  marketplaceSingletons ::
    Array
      ( Tuple (Tuple String (Either Sale (JsonTuple ThreadToken (JsonTuple PubKeyHash (JsonTuple Value POSIXTime)))))
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
          Right (JsonTuple (Tuple threadToken (JsonTuple (Tuple pubKeyHash (JsonTuple (Tuple value pOSIXTime)))))) ->
            Right
              { threadToken: threadToken
              , owner: pubKeyHash
              , value: value
              , endTime: pOSIXTime
              }
    }
