-- Palindromic sums

{-

-}

module Problem125
  ( problem125
  , palindromicSum
  ) where

problem125 :: IO ()
problem125 = print $ palindromicSum (round 1e8)

palindromicSum :: Integer -> Integer
palindromicSum nMax = 42

