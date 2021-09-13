module Utils.ByteString where

import Prelude
import Data.Array (catMaybes, reverse, snoc)
import Data.Char (fromCharCode)
import Data.Int (fromStringAs, hexadecimal)
import Data.String (drop, length, take)
import Data.String.CodeUnits (fromCharArray)

-- TODO this function only works for punctuation and latin
decodeUtf8 :: String -> String
decodeUtf8 bs = fromCharArray $ catMaybes $ map toChar $ splitByTwo bs
  where
  toChar = fromStringAs hexadecimal >=> fromCharCode

splitByTwo :: String -> Array String
splitByTwo = go >>> reverse
  where
  go :: String -> Array String
  go s
    | length s > 0 = go (drop 2 s) `snoc` take 2 s
    | otherwise = []
