-- Pandigital prime sets

{-

1. Test for pandigital primes. 8P8 = 40320.

   There are no 9-digit pd primes, because sum [1..9] = 45. There
   happen to be no 8-digit pd primes either.

map (length . filter isPrime . map digitsToInt . nPkEnum [1..9]) [1..9]
[4,14,48,205,576,1402,2807,0,0]  -- 5056 primes total


2. Coin partitions with limited coins (digits)
   [1,1,1,1,2,2,2,2,3,3,3,4,4,5,6,7]
   There are only 4 single digit primes. Total digits must sum to 9.
   There are 23 partitions that sum to 9.
   
> sort $ parts 9 (reverse nDigs)
[[1,1,1,1,2,3],[1,1,1,1,5],[1,1,1,2,2,2],[1,1,1,2,4],[1,1,1,3,3],[1,1,1,6],[1,1,2,2,3],[1,1,
2,5],[1,1,3,4],[1,1,7],[1,2,2,2,2],[1,2,2,4],[1,2,3,3],[1,2,6],[1,3,5],[1,4,4],[2,2,2,3],[2,
2,5],[2,3,4],[2,7],[3,3,3],[3,6],[4,5]]




-}

module Problem118
  ( problem118
  , answer
  ) where

import Data.List (group, nub, sort)
import PE.Primes
import PE.Utils

problem118 :: IO ()
problem118 = print $ answer 100

nDigs = [1,1,1,1,2,2,2,2,3,3,3,4,4,5,6,7]

-- Index by !! nDigits
pdPrimes = [] : map (sort . filter isPrime . map digitsToInt . nPkEnum [1..9]) [1..7]

-- The partitions from ds that sum to s. For example, parts 3 nDigs
-- returns [[1,1,1],[2,1],[3]].
parts s = nub . parts' [] s where
  parts' ps 0 _ = [ps]
  parts' _ _ [] = []
  parts' ps s (d:ds)
    | s < 0 = []
    | otherwise = parts' (d:ps) (s-d) ds ++
                  parts' ps s ds

-- Convert a list to a summary assoc list, e.g., [1,1,1,1,2,3] ->
-- [(1,4),(2,1),(3,1),(4,0)...(7,0)].
l2al :: Integral i => [i] -> [(i, Int)]
l2al = gl2al . group

gl2al dss = reverse . fst $ foldr f ([], dss) [7,6..1] where
  f d (al, []) = ((d, 0):al, [])
  f d (al, (ds:dss))
    | head ds == d = ((d, length ds):al, dss)
    | otherwise = ((d, 0):al, ds:dss)

-- The partitions in assoc list form, zero filled.
partsal = map l2al (sort $ parts 9 (reverse nDigs))

-- 
enumPrimeSets part = [ d1 ++ d2 ++ d3 ++ d4 ++ d5 ++ d6 ++ d7 |
                       let (Just n1) = lookup 1 part,
                       let (Just n2) = lookup 2 part,
                       let (Just n3) = lookup 3 part,
                       let (Just n4) = lookup 4 part,
                       let (Just n5) = lookup 5 part,
                       let (Just n6) = lookup 6 part,
                       let (Just n7) = lookup 7 part,
                       d1 <- nCkEnum (pdPrimes !! 1) n1,
                       d2 <- nCkEnum (pdPrimes !! 2) n2,   
                       d3 <- nCkEnum (pdPrimes !! 3) n3,   
                       d4 <- nCkEnum (pdPrimes !! 4) n4,   
                       d5 <- nCkEnum (pdPrimes !! 5) n5,   
                       d6 <- nCkEnum (pdPrimes !! 6) n6,   
                       d7 <- nCkEnum (pdPrimes !! 7) n7 ]   

{- 
λ> length $ concatMap enumPrimeSets partsal
4935838

λ> elem [2,5,47,89,631] enumPDPrimeSets 
True

λ> length enumPDPrimeSets 
33197

-}

isPandigital ps = length ds == 9 where
  ds = nub $ concatMap digits ps

-- length 33197 wrong
enumPDPrimeSets = filter isPandigital $ concatMap enumPrimeSets partsal


-- to be removed
answer :: Integral i => i -> i
answer pow = sum $ map digitsSum [1 .. pow]

digitsSum :: Integral i => i -> i
digitsSum pow = sum0 + 2 * sum1to9 - 9
  where sum0 = nCk (pow + 9 - 1) 9 - 1
        sum1to9 = sum $ map (\n -> nCk (n + pow - 2) (pow - 1)) [1..9] 

