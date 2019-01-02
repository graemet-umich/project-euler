-- Reciprocal cycles

{-
The solution is the largest full reptend prime less than 1000. By
comparison, the longest cycle for a non-full reptend prime less than
1000, p = 991, is 495.

A full reptend prime p has a 1/p cycle length of p-1 digits. For
example, 1/7 = 0.(142857).

If p is a full reptend prime, then the multiplicative order of 10
modulo p is equal to p-1.

The multiplicative order of b modulo n is the smallest positive
integer k with
     b^k === 1 (mod n).
-}

module Problem026
    ( problem026
    , isFullReptendPrime
    ) where

import Data.List (sort)
import Data.Numbers.Primes (primes)

problem026 :: IO ()
problem026 =
  print .
  head .
  filter isFullReptendPrime .
  reverse .
  takeWhile (< 1000) $
  primes

isFullReptendPrime :: Integral a => a -> Bool
isFullReptendPrime p = multOrd p == p - 1

-- Only use for p prime, which is sufficient for this problem. (If p
-- is not prime, then one needs to map over the factors of phi(p),
-- Euler's totient funtion.)
multOrd :: Integral a => a -> a
multOrd p =
  fst .
  head .
  filter (\(_, mo) -> mo == 1) .
  map (\i -> (i, mod (10^i) p)) $
  factors $ (p - 1)

factors :: Integral a => a -> [a]
factors n = sort $ factors' 1 [] where
  factors' i xs
    | i * i > n = xs
    | i * i == n = i : xs
    | r == 0 = factors' (i + 1) (i : q : xs)
    | otherwise = factors' (i + 1) xs
    where (q, r) = quotRem n i
