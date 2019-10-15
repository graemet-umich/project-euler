-- Repunit nonfactors

{-

What is the smallest value of n to use in R(10^n) for this problem? Let
k = 10^n.

From problem 132, a prime p does not divide R(k) if
  10^(gcd k (p-1)) /= 1 (mod p).

Looking at the test case for p = 17, (p - 1) = 16 = 2^4. The least k
that has 2^4 as a factor is 10^4.

Likewise, the largest p less than 10^5 such that (p - 1) has the
greatest number of 2's as prime factors is 65537, where 65536 = 2^16.
Hence the least k is 10^16.

-}

module Problem133
  ( problem133
  , primeNonFactorsRk
  ) where

import Data.Numbers.Primes (primes)
import PE.Utils (modPow)

maxPrime = 100000

problem133 :: IO()
problem133 = print $ soln133 (10^16)

soln133 = sum . primeNonFactorsRk

primeNonFactorsRk :: Integer -> [Integer]
primeNonFactorsRk k =
  (++) [2, 3, 5] .
  filter (\p -> modPow 10 (gcd k (p - 1)) p /= 1) .
  takeWhile (< maxPrime) $
  drop 3 primes


