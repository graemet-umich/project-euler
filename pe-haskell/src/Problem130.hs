-- Repunit divisibility

{-

Given function A from problem 129, this solution is straightforward.

-}

module Problem130
  ( problem130
  , composites
  ) where

import Data.Numbers.Primes
import PE.Repunit (_A, nsOver)

problem130 :: IO()
problem130 = print . sum $ composites 25

-- "False prime" composites that satisfy pPred
composites :: Int -> [Integer]
composites n = take n . filter pPred . filter (not . isPrime) $ nsOver 1

-- For all primes p > 5, A(p) | (p - 1).
pPred :: Integer -> Bool
pPred p = (p - 1) `mod` _A p == 0
