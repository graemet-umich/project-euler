-- Lexicographic permutations

{-
Data.List.permutations does not preserve ordering, so a subsequent
sort is required.

The sort does take about 98% of the execution time.

Implemented an orderedPermutations function that preserves ordering,
so a sort is not necessary, as long as the argument is already sorted.
-}

module Problem024
    ( problem024
    , nthLexPerm
    ) where

import Data.List (sort)

problem024 :: IO ()
problem024 = print $ nthLexPerm 1000000 "0123456789"

nthLexPerm :: Ord a => Int -> [a] -> [a]
nthLexPerm n =
  head .
  drop (n - 1) .
  orderedPermutations

-- Note this function fails if there is any degeneracy in the xs,
-- which there is not here.
orderedPermutations :: Eq a => [a] -> [[a]]
orderedPermutations [] = [[]]
orderedPermutations xs = concatMap f xs where
  f x = map (x:) $ orderedPermutations (bs ++ es) where
    bs = takeWhile (/= x) xs
    (_:es) = dropWhile (/= x) xs
