module Data.Json.JsonTuple where

import Prelude

import Data.Argonaut (JsonDecodeError(..))
import Data.Argonaut as A
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Error as Error
import Data.Argonaut.Decode.Generic (genericDecodeJson, genericDecodeJsonWith)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson, genericEncodeJsonWith)
import Data.Argonaut.Types.Generic (Encoding)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Foreign (ForeignError(..), fail, readArray)
import Foreign.Class (class Decode, class Encode, decode, encode)
import Web.HTML.Event.EventTypes (offline)
import Web.HTML.HTMLFormElement (reset)

newtype JsonTuple a b
  = JsonTuple (Tuple a b)

derive instance newtypeJsonTuple :: Newtype (JsonTuple a b) _

derive instance eqJsonTuple :: (Eq a, Eq b) => Eq (JsonTuple a b)

derive instance ordJsonTuple :: (Ord a, Ord b) => Ord (JsonTuple a b)

derive instance genericJsonTuple :: Generic (JsonTuple a b) _

derive instance functorJsonTuple :: Functor (JsonTuple a)

instance showJsonTuple :: (Show a, Show b) => Show (JsonTuple a b) where
  show = genericShow

instance encodeJsonTuple :: (Encode a, Encode b) => Encode (JsonTuple a b) where
  encode (JsonTuple (Tuple a b)) = encode [ encode a, encode b ]

instance decodeJsonTuple :: (Decode a, Decode b) => Decode (JsonTuple a b) where
  decode value = do
    elements <- List.fromFoldable <$> readArray value
    consume elements
    where
    consume Nil = fail $ ForeignError "Decoding a JsonTuple, expected to see an array with exactly 2 elements, got 0"

    consume (Cons x Nil) = fail $ ForeignError "Decoding a JsonTuple, expected to see an array with exactly 2 elements, got 1."

    consume (Cons x (Cons y Nil)) = do
      a <- decode x
      b <- decode y
      pure $ JsonTuple (Tuple a b)

    consume (Cons x ys) = do
      a <- decode x
      b <- decode $ encode $ Array.fromFoldable ys
      pure $ JsonTuple (Tuple a b)

instance encodeJsonJsonTuple :: (EncodeJson a, EncodeJson b) => EncodeJson (JsonTuple a b) where
  encodeJson (JsonTuple (Tuple a b)) = encodeJson [ encodeJson a, encodeJson b ]

instance decodeJsonJsonTuple :: (DecodeJson a, DecodeJson b) => DecodeJson (JsonTuple a b) where
  decodeJson value = do
    let res = List.fromFoldable <$> A.toArray value
    case res of
      Nothing       -> Left $ Error.TypeMismatch "Decoding a JsonTuple, expected an array, got something else."
      Just elements -> consume elements
    where
    consume Nil = Left $ Error.TypeMismatch "Decoding a JsonTuple, expected to see an array with exactly 2 elements, got 0"

    consume (Cons x Nil) = Left $ Error.TypeMismatch "Decoding a JsonTuple, expected to see an array with exactly 2 elements, got 1."

    consume (Cons x (Cons y Nil)) = do
      let a = decodeJson x
      let b = decodeJson y
      case (a /\ b) of
        (Right a /\ Right b) -> pure $ JsonTuple (a /\ b)
        (Left e /\  _)       -> Left e
        (_ /\ Left e)        -> Left e

    consume (Cons x ys) = do
      let a = decodeJson x
      let b = decodeJson $ encodeJson $ Array.fromFoldable ys
      case (a /\ b) of
        (Right a /\ Right b) -> pure $ JsonTuple (a /\ b)
        (Left e /\  _)       -> Left e
        (_ /\ Left e)        -> Left e

_JsonTuple :: forall k v. Iso' (JsonTuple k v) (Tuple k v)
_JsonTuple = _Newtype
