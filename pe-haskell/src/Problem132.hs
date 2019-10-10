-- Large repunit factors

{-

Use the following arguement to avoid calculating A(p), which is
expensive.

The repunit R(k) is
  10^k - 1 / 9.
The problem asks to find
  10^k - 1 / 9 = 0 (mod p),
which is equivalent to
  10^k = 1 (mod p).

Only p > 5 can satisfy this equation for k = 10^n, because 2 and 5 are
not coprime with 10, and 3 does not divide R(k) (unless 3 divides k,
which is not the case here).

Using Fermat's Little Theorem,
  10^(p - 1) = 1 (mod p),
for prime p.

If
  d c1 = k
then
  10^(d c1) = (10^d)^c1 = 1^c1 (mod p)
  10^d = 1 (mod p)
and if
  d c2 = (p - 1)
then
  10^(d c2) = (10^d)^c2 = 1^c2 (mod p)
  10^d = 1 (mod p).
The smallest such d is gcd k (p - 1), so the problem can be recast as
  10^(gcd k (p - 1)) = 1 (mod p).

By the way, from problem 130,
  A(p) | (p - 1) or
  A(p) c3 = (p - 1)
for p > 5, so
  10^A(p) = 1 (mod p).

-}

module Problem132
  ( problem132
  , soln132
  ) where

import Data.Numbers.Primes (primes)
import PE.Utils (modPow)

problem132 :: IO()
problem132 = print $ soln132 40 (10^9)

soln132 n = sum . take n . primeFactorsRk

primeFactorsRk :: Integer -> [Integer]
primeFactorsRk k =
  filter (\p -> modPow 10 (gcd k (p - 1)) p == 1) $
  drop 3 primes
