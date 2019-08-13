-- Utils.hs
-- Utility functions

module PE.Utils
  (
    -- Combinatorics
    nCkEnum
  , nPkEnum
    -- Utilities
  , modExp
  , modPow
  , digits
  , digitsToInt
  , split
  ) where

import Data.Char (digitToInt)
import Data.List (permutations, tails)

-----
-- Combinatorics

-- Also known as enumerate n choose k.
-- Enumerate all k length combinations of xs. 
nCkEnum :: [a] -> Int -> [[a]]
nCkEnum [] _ = [[]]
nCkEnum  _ 0 = [[]]
nCkEnum xs k 
  | n < k = []
  | otherwise = concatMap f .
                take (n - k + 1) .
                map (\ts -> (ts, k)) .
                tails $ xs
  where n = length xs
        f (y:ys, k') = map (y:) $ nCkEnum ys (k' - 1)

-- Enumerate all k length permutations of xs. NB length nPn = n! =
-- number of permutations of n elements.
nPkEnum :: [a] -> Int -> [[a]]
nPkEnum xs = concatMap permutations . nCkEnum xs

-- Raise b to the power of e modulus modMax.
-- b^e mod modMax
modExp :: Integer -> Integer -> Integer -> Integer
modExp b e modMax
  | e == 0 = 1
  | e == 1 = b
  | even e = mod (expbd * expbd) modMax
  | otherwise = mod (expbd * expbd1) modMax
  where d = div e 2
        expbd  = modExp b d modMax
        expbd1 = modExp b (d + 1) modMax

modPow = modExp

-----
-- Utilities

-- Convert an integer to a list of digits.
digits :: (Integral int, Show int) => int -> [Int]
digits = map digitToInt . show

-- Convert a list of digits to an integer.
digitsToInt :: (Integral int) => [int] -> int
digitsToInt [] = 0
digitsToInt digits = go 0 digits where
  go n [] = n
  go n (d:ds) = go (10 * n + d) ds

-- Split string s on c 0 or more times. For example,
--      split ',' ",yo,,yo,,"
--      ["","yo","","yo","",""]
split :: Char -> String -> [String]
split c s = split' s []
  where split' s acc
          | s  == "" = acc
          | s  == [c] = acc ++ [""] ++ [""]  -- from more than 1 trailing c
          | s2 == "" = acc ++ [s1]  -- end
          | otherwise = split' (tail s2) (acc ++ [s1])
          where s1 = takeWhile (/= c) s
                s2 = dropWhile (/= c) s
