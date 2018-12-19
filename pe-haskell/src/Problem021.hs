-- Amicable numbers

{-
Note Thâbit ibn Kurrah's Rule and Euler's Rule are sufficient but not
necessary for amicability; these rules do not generate all amicable
pairs.
-}

module Problem021
    ( problem021
    , getAmicablePair
    ) where

import Data.List (group)
import Data.Numbers.Primes (primeFactors)

maxN = 10000

-- Although not the case with maxN here, this function filters for
-- amicable numbers less than maxN. For example, if maxN was 250, then
-- 220 would be included and 284 would not.
problem021 :: IO ()
problem021 = print .
             sum .
             filter (< maxN) .
             concatMap getAmicablePair $
             [1 .. maxN - 1]

-- Return [a, b] if a < b and a & b are an amicable pair, else [].
getAmicablePair :: Integral a => a -> [a]
getAmicablePair n
  | n' <= n = []  -- equality test removes perfect numbers
  | n'' == n = [n, n']
  | otherwise = []
  where n'  = sumProperDivisors n
        n'' = sumProperDivisors n'

sumProperDivisors :: Integral a => a -> a
sumProperDivisors n = sumDivisors n - n

-- Let n = p1^e1 * p2^e2 * ... * pm^em, where each pi is a prime factor of n.
-- Let ni = pi^ei, so n = n1 * n2 * ... * nm
-- The sum of the divisors of ni:
--    S(ni) = 1 + pi + ... + pi^ei
--          = (pi^(ei+1) - 1) / (pi - 1)
-- S(n) = S(n1) * S(n2) * ... * S(em)
sumDivisors :: Integral a => a -> a
sumDivisors = product .
              map (\ps@(p:_) -> quot (product (p : ps) - 1) (p - 1)) .
              group . primeFactors
