-- Repunit divisibility

{-

Given function A from problem 129, this solution is straightforward.

-}

module Problem130
  ( problem130
  , composites
  ) where

import Data.Numbers.Primes

problem130 :: IO()
problem130 = print . sum $ composites 25

composites :: Int -> [Integer]
composites n = take n . filter pPred . filter (not . isPrime) $ nsOver 1

pPred :: Integer -> Bool
pPred p = (p - 1) `mod` _A p == 0
  
_A :: Integer -> Integer
_A n = go 1 1 where
  go k r
    | rmn == 0 = k
    | otherwise = go (k + 1) (10 * rmn + 1)
    where rmn = r `mod` n

-- n >= nMin >= 0 | gcd(10,n) = 1
nsOver :: Integer -> [Integer]
nsOver nMin = dropWhile (<= nMin) .
              concatMap (\i -> map (+ i) [1,3,7,9]) $
              [n10, n10 + 10 ..]
  where n10 = 10 * (nMin `div` 10) 
