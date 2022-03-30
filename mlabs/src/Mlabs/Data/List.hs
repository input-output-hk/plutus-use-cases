-- | Missing plutus functions for Lists
module Mlabs.Data.List (
  take,
  sortOn,
  sortBy,
  mapM_,
  firstJustRight,
  maybeRight,
) where

import PlutusTx.Prelude hiding (mapM_, take)
import Prelude qualified as Hask (Monad, seq)

import Mlabs.Data.Ord (comparing)

{-# INLINEABLE take #-}

{- | 'take' @n@, applied to a list @xs@, returns the prefix of @xs@
 of length @n@, or @xs@ itself if @n > 'length' xs@.

 >>> take 5 "Hello World!"
 "Hello"
 >>> take 3 [1,2,3,4,5]
 [1,2,3]
 >>> take 3 [1,2]
 [1,2]
 >>> take 3 []
 []
 >>> take (-1) [1,2]
 []
 >>> take 0 [1,2]
 []

 It is an instance of the more general 'Data.List.genericTake',
 in which @n@ may be of any integral type.
-}
take :: Integer -> [a] -> [a]
take n
  | n <= 0 = const []
  | otherwise = \case
    [] -> []
    a : as -> a : take (n - 1) as

{-# INLINEABLE sortOn #-}

{- | Sort a list by comparing the results of a key function applied to each
 element.  @sortOn f@ is equivalent to @sortBy (comparing f)@, but has the
 performance advantage of only evaluating @f@ once for each element in the
 input list.  This is called the decorate-sort-undecorate paradigm, or
 Schwartzian transform.

 Elements are arranged from lowest to highest, keeping duplicates in
 the order they appeared in the input.

 >>> sortOn fst [(2, "world"), (4, "!"), (1, "Hello")]
 [(1,"Hello"),(2,"world"),(4,"!")]
-}
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `Hask.seq` (y, x))

{-# INLINEABLE mapM_ #-}
mapM_ :: Hask.Monad f => (a -> f ()) -> [a] -> f ()
mapM_ f = \case
  [] -> return ()
  a : as -> do
    _ <- f a
    mapM_ f as

{-# INLINEABLE firstJustRight #-}
firstJustRight :: (a -> Maybe (Either b c)) -> [a] -> Maybe c
firstJustRight f = \case
  (x : xs) ->
    case f x of
      Just (Right a) -> Just a
      _ -> firstJustRight f xs
  [] ->
    Nothing

{-# INLINEABLE maybeRight #-}
maybeRight :: Either a b -> Maybe b
maybeRight = either (const Nothing) Just
