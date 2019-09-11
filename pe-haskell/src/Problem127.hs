-- abc-hits

{-

The brute force number of (a,b,c) triplets to solve this problem is
O(n^3) 1.2e5 ^ 3. Imposing predicates 2 and 3 reduces the space to O(2)
= (n-1)(n-2)/2 = 7.2e9. This is still too large.

From the definition of rad, each prime factor must be unique, so
     rad(abc) = rad(a) * rad(b) * rad(c).
From predicate 4,
     rad(a) * rad(b) * rad(c) < c.
Because a < b, then b >= 2, so
     rad(b) >= 2 and
     rad(a) * 2 < c / rad(c).

Problem 124, Ordered radicals, required sorting radicals, so select
all rad(a)'s that satisfy this last inequality, then lookup the
corresponding a's such that
     a < c / 2,
because a + b = c and a < b. The resulting triplet space is reduced to
5.5e6.

A further space reduction to 2.6e6 is achieved by noting that when c
is even, rad(a) must be odd for gcd(a,c) == 1. Testing for even c is
faster than testing for the gcd.

Finally, predicate 1 is satisfied by just gcd(a,b) = 1. If gcd(a,b) =
1, then gcd(a,c) = 1 and gcd(b,c) = 1.

Prove the contrapositive, if gcd(a,c) /= 1 then gcd(a,b) /= 1:
Suppose gcd(a,c) = d /= 1, then
     gcd(a'd, c'd) = d.
From predicate 3,
     b = c - a
       = (c' - a')d
       = b'd,
so
     gcd(a'd,b'd) = gcd(a,b) = d /= 1. Qed.
This proof also holds for gcd(b,c) = 1.

A note on testing: function is_abc_hit is tested, but it is not used
for the solution. The solution uses the pre-calculated values of c and
rad(c) for an equivalent result.

-}

module Problem127
  ( problem127
  , is_abc_hit
  , abc_hits
  , sum_cs_abc_hits
  ) where

import Data.Array
import Data.List (group, groupBy, sortBy)
import Data.Numbers.Primes (primeFactors)

cMax = 120000

problem127 :: IO ()
problem127 = do
  putStrLn $ "cMax = " ++ (show cMax)
  print $ sum_cs_abc_hits cMax

type ABC = (Integer, Integer, Integer)

sum_cs_abc_hits :: Integer -> Integer
sum_cs_abc_hits = sum . map (\(_,_,c) -> c) . abc_hits

is_abc_hit :: ABC -> Bool
is_abc_hit abc = pred1 abc  && pred4 abc 

abc_hits :: Integer -> [ABC]
abc_hits cMax = [ (a, b, c) |
                  c <- [9 .. cMax-1],
                  let rc = rad!c,
                  let crc = c `div` rc,
                  let ras' = takeWhile (\ra -> 2 * ra < crc) [1..],
                  let ras = if odd c then ras' else filter odd ras', 
                  -- predicate 2:
                  a <- filter (< ((c - 1) `div` 2)) $ concatMap (sortRad!) ras,
                  -- predicate 3:
                  let b = c - a,
                  -- predicate 4:
                  rad!a * rad!b < crc,
                  -- predicate 1: (for speed reading)
                  pred1 (a,b,c) ]

-- distinct prime factors of n
dpfs :: Integer -> [Integer]
dpfs = map head . group . primeFactors

pred1 :: ABC -> Bool
pred1 (a,b,_) = gcd a b == 1

pred4 :: ABC -> Bool
pred4 abc@(a,b,c) = rad!a * rad!b * rad!c < c

-- Given n, return rad(n).
radL :: [(Integer, Integer)]
radL  = [ (n, product (dpfs n)) | n <- [1..cMax] ]
rad   = array (1,cMax) radL

-- Given rad(n), lookup all corresponding n. For example, if rad(n) =
-- 3, then n = [3,6..] = 3^i where i > 0. Only cMax / 2 terms are
-- required (see predicate 2 in abc_hits).
sortRadL :: [(Integer, [Integer])]
sortRadL = map f .
           groupBy (\(a,_) (b,_) -> a == b) .
           map (\(x,i) -> (i,x)) .
           sortBy (\(_,a) (_,b) -> compare a b) .
           filter (\(n,_) -> n <= cMax `div` 2) $
           radL
  where
    f rns@((r,_):_) = (r, map (\(_,n) -> n) rns)
sortRad = array (1, cMax `div` 2)
                (zip [1 .. cMax `div` 2] $ repeat []) // sortRadL
