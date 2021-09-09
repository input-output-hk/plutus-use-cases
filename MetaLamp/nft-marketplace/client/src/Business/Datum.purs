module Business.Datum where

import Prelude
import Data.Array (snoc)
import Data.BigInteger (fromInt)
import Data.Foldable (foldr)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), snd)
import Plutus.Contracts.NftMarketplace.OnChain.Core.StateMachine (MarketplaceDatum)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Value (CurrencySymbol, TokenName(..), Value)
import PlutusTx.AssocMap as AssocMap

type NftSingleton
  = { ipfsCid :: String
    , currency :: CurrencySymbol
    , name :: String
    , description :: String
    , category :: Array String
    , issuer :: Maybe PubKeyHash
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
          , name: record.niName
          , description: record.niDescription
          , category: record.niCategory
          , issuer: record.niIssuer
          }
