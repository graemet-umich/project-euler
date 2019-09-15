-- Repunit divisibility

{-

The ns can only end in 1, 3, 7, or 9. The first n is 3, because
gcd(1,n) = 1.

-}

module Problem129
  ( problem129,
    _A,
    iAexceeds
  ) where

import Data.Numbers.Primes

problem129 :: IO()
problem129 = print $ iAexceeds 100

iAexceeds :: Integer -> Integer
iAexceeds k = fst . head .
              dropWhile (\(_, an) -> an <= k) .
              map (\n -> (n, _A n)) $ ns

_A :: Integer -> Integer
_A n = fst . head .
       dropWhile (\(_, rkmn) -> rkmn /= 0) .
       map (\k -> (k, _R k `mod` n)) $ [2..]

-- n > 1 | gcd(10,n) = 1
ns :: [Integer]
ns = tail $ concatMap (\i -> map (+ i) [1,3,7,9]) [0,10..]

_R :: Integer -> Integer
_R k = (10^k - 1) `div` 9

