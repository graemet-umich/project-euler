-- Repunit.hs
-- Functions associated with repeated unit numbers

{-

A repunit of length k, R(k), is a number consisting of k 1's.

Given that n > 0 and
  gcd(n, 10) = 1 (n and 10 are coprime),
then for all k,
  n | R(k).
Let A(n) be the least such value of k.


-}

module PE.Repunit
  ( _A
  , nsOver
  ) where

-- Long division efficiently finds A(n).
_A :: Integer -> Integer
_A n = go 1 1 where
  go k r
    | rmn == 0 = k
    | otherwise = go (k + 1) (10 * rmn + 1)
    where rmn = r `mod` n

-- Enumerate all n greater than nMin that are coprime to 10.
-- n >= nMin >= 0 | gcd(10,n) = 1
nsOver :: Integer -> [Integer]
nsOver nMin = dropWhile (<= nMin) .
              concatMap (\i -> map (+ i) [1,3,7,9]) $
              [n10, n10 + 10 ..]
  where n10 = 10 * (nMin `div` 10) 
