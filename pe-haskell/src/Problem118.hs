-- Pandigital prime sets

{-

1. Test for pandigital primes. 

   There are no 9-digit pd primes, because sum [1..9] = 45.

   There are 23082 8-digit primes out of 9P8 = 362880 permutations (~6%).

λ> map (length . filter isPrime . map digitsToInt . nPkEnum [1..9]) [1..9]
[4,20,83,395,1610,5045,12850,23082,0]
(64.08 secs, 88,049,405,424 bytes)
λ> sum it
43089

2. "Coin partitions" style with limited coins (digits)
   [1,1,1,1,2,2,2,2,3,3,3,4,4,5,6,7,8]
   There are only 4 single digit primes. Total digits must sum to 9.
   There are 24 partitions that sum to 9.
   
λ> sort $ parts 9 (reverse nDigs)
[[1,1,1,1,2,3],[1,1,1,1,5],[1,1,1,2,2,2],[1,1,1,2,4],[1,1,1,3,3],[1,1,1,6],[1,1,2,2,3],[1,1,
2,5],[1,1,3,4],[1,1,7],[1,2,2,2,2],[1,2,2,4],[1,2,3,3],[1,2,6],[1,3,5],[1,4,4],[1,8],[2,2,2,
3],[2,2,5],[2,3,4],[2,7],[3,3,3],[3,6],[4,5]]                                              
(0.02 secs, 1,556,312 bytes)
λ> length it
24


-}

module Problem118
  ( problem118
  , enumPDPrimeSets
  ) where

import Data.List (group, nub, sort)
import PE.Primes
import PE.Utils

problem118 :: IO ()
problem118 = print answer

-- A list of 8 pandigital prime sets, and each set contains primes
-- with the same number of digits. For example, pdPrimes !! 5 is the
-- set of all 5-digit pandigital primes.
pdPrimes = [] : map (sort . filter isPrime . map digitsToInt . nPkEnum [1..9]) [1..8]

-- The digit counts used to sum to 9, for enumeration of the 24 partitions.
nDigs = [1,1,1,1,2,2,2,2,3,3,3,4,4,5,6,7,8]

-- The partitions from ds that sum to s. For example, parts 3 nDigs
-- returns [[1,1,1],[2,1],[3]].
parts :: Int -> [Int] -> [[Int]]
parts s = nub . parts' [] s where
  parts' ps 0 _ = [ps]
  parts' _ _ [] = []
  parts' ps s (d:ds)
    | s < 0 = []
    | otherwise = parts' (d:ps) (s-d) ds ++
                  parts' ps s ds

-- Convert a list to a summary assoc list, e.g., [1,1,1,1,2,3] ->
-- [(1,4),(2,1),(3,1),(4,0)...(8,0)].
l2al :: Integral i => [i] -> [(i, Int)]
l2al = gl2al . group

gl2al dss = reverse . fst $ foldr f ([], dss) [8,7..1] where
  f d (al, []) = ((d, 0):al, [])
  f d (al, (ds:dss))
    | head ds == d = ((d, length ds):al, dss)
    | otherwise = ((d, 0):al, ds:dss)

-- The 24 partitions in assoc list form, zero filled.
partsal :: [[(Int, Int)]]
partsal = map l2al (sort $ parts 9 (reverse nDigs))

-- The enumeration of all prime sets for a given partition part.
enumPrimeSets :: [(Int, Int)] -> [[Int]]
enumPrimeSets part = [ d1 ++ d2 ++ d3 ++ d4 ++ d5 ++ d6 ++ d7 ++ d8 |
                       let (Just n1) = lookup 1 part,
                       let (Just n2) = lookup 2 part,
                       let (Just n3) = lookup 3 part,
                       let (Just n4) = lookup 4 part,
                       let (Just n5) = lookup 5 part,
                       let (Just n6) = lookup 6 part,
                       let (Just n7) = lookup 7 part,
                       let (Just n8) = lookup 8 part,
                       d1 <- nCkEnum (pdPrimes !! 1) n1,
                       d2 <- nCkEnum (pdPrimes !! 2) n2,   
                       d3 <- nCkEnum (pdPrimes !! 3) n3,   
                       d4 <- nCkEnum (pdPrimes !! 4) n4,   
                       d5 <- nCkEnum (pdPrimes !! 5) n5,   
                       d6 <- nCkEnum (pdPrimes !! 6) n6,   
                       d7 <- nCkEnum (pdPrimes !! 7) n7,   
                       d8 <- nCkEnum (pdPrimes !! 8) n8 ]   

isPandigital :: [Int] -> Bool
isPandigital ps = length ds == 9 where
  ds = nub $ concatMap digits ps

enumPDPrimeSets :: [(Int, Int)] -> [[Int]]
enumPDPrimeSets = filter isPandigital . enumPrimeSets

answer :: Int
answer = length $ concatMap enumPDPrimeSets partsal 
