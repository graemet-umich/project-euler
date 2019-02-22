-- Coin sums

{-
I first saw this problem as an exercise in SICP.

By starting with the largest denominations, Haskell effectively
memoizes the function coinSums', needing 4x less memory (fewer
different coinSums' arguments) and running 5x faster.
-}

module Problem031
  ( problem031
  , coinSums
  ) where

problem031 :: IO ()
problem031 = print $ coinSums 200 denominations

denominations = reverse [1, 2, 5, 10, 20, 50, 100, 200]

coinSums :: Integral int => int -> [int] -> int
coinSums 0 _ = 0
coinSums amount denominations = coinSums' amount denominations where
  coinSums' 0 _ = 1
  coinSums' _ [] = 0
  coinSums' amt (d:ds)
    | amt < 0 = 0
    | otherwise = coinSums' (amt - d) (d:ds) + coinSums' amt ds
