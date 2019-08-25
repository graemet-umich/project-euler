-- Ordered radicals

{-
Use a brute force approach.
-}

module Problem124
  ( problem124
  , _E
  ) where

import Data.List (group, sort)
import Data.Numbers.Primes (primeFactors)

problem124 :: IO ()
problem124 = print $ _E 100000 10000

_E :: Int -> Int -> Int
_E nMax k = snd .
            head .
            drop (k - 1) .
            sort $
            map (\n -> (rad n, n)) [1 .. nMax]

rad :: Int -> Int
rad = product . map head . group . primeFactors 
