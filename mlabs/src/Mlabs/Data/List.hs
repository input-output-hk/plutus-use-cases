-- | Missing plutus functions for Lists
module Mlabs.Data.List(
    take
  , sortOn
  , sortBy
  , mapM_
) where

import PlutusTx.Prelude hiding (take, mapM_)

import Mlabs.Data.Ord (comparing)

{-# INLINABLE take #-}
-- | 'take' @n@, applied to a list @xs@, returns the prefix of @xs@
-- of length @n@, or @xs@ itself if @n > 'length' xs@.
--
-- >>> take 5 "Hello World!"
-- "Hello"
-- >>> take 3 [1,2,3,4,5]
-- [1,2,3]
-- >>> take 3 [1,2]
-- [1,2]
-- >>> take 3 []
-- []
-- >>> take (-1) [1,2]
-- []
-- >>> take 0 [1,2]
-- []
--
-- It is an instance of the more general 'Data.List.genericTake',
-- in which @n@ may be of any integral type.
take :: Integer -> [a] -> [a]
take n
  | n <= 0    = const []
  | otherwise = \case
      []   -> []
      a:as -> a : take (n - 1) as

{-# INLINABLE sortOn #-}
-- | Sort a list by comparing the results of a key function applied to each
-- element.  @sortOn f@ is equivalent to @sortBy (comparing f)@, but has the
-- performance advantage of only evaluating @f@ once for each element in the
-- input list.  This is called the decorate-sort-undecorate paradigm, or
-- Schwartzian transform.
--
-- Elements are arranged from lowest to highest, keeping duplicates in
-- the order they appeared in the input.
--
-- >>> sortOn fst [(2, "world"), (4, "!"), (1, "Hello")]
-- [(1,"Hello"),(2,"world"),(4,"!")]
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))

{-# INLINABLE sortBy #-}
-- | The 'sortBy' function is the non-overloaded version of 'sort'.
--
-- >>> sortBy (\(a,_) (b,_) -> compare a b) [(2, "world"), (4, "!"), (1, "Hello")]
-- [(1,"Hello"),(2,"world"),(4,"!")]
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp = mergeAll . sequences
  where
    sequences (a:b:xs) = case a `cmp` b of
      GT -> descending b [a]  xs
      _  -> ascending  b (a:) xs
    sequences xs = [xs]

    descending a as (b:bs) = case a `cmp` b of
      GT -> descending b (a:as) bs
      _  -> (a:as): sequences bs
    descending a as bs  = (a:as): sequences bs

    ascending a as (b:bs) = case a `cmp` b of
      GT -> let !x = as [a]
            in x : sequences bs
      _  -> ascending b (\ys -> as (a:ys)) bs
    ascending a as bs   = let !x = as [a]
                          in x : sequences bs

    mergeAll [x] = x
    mergeAll xs  = mergeAll (mergePairs xs)

    mergePairs (a:b:xs) = let !x = merge a b
                          in x : mergePairs xs
    mergePairs xs       = xs

    merge as@(a:as') bs@(b:bs') = case a `cmp` b of
      GT -> b:merge as  bs'
      _  -> a:merge as' bs
    merge [] bs         = bs
    merge as []         = as


{-# INLINABLE mapM_ #-}
mapM_ :: Monad f => (a -> f ()) -> [a] -> f ()
mapM_ f = \case
  []   -> return ()
  a:as -> do
    _ <- f a
    mapM_ f as

