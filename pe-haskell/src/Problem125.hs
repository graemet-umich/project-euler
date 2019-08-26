-- Palindromic sums

{-
Enumerate all "sums of consecutive squares" less than nMax, where nMax
= 1000 for the test case and nMax = 10^8 for the problem. Remove
duplicates using a set. Filter for palindromic sums. Sum palindromic
sums.

For nMax = 10^8, the shortest sum is 7070^2 + 7071^2 = 99,983,941, and
the longest sum is 1^2 + 2^2 + ... + 668^2 = 99,582,434.

There are 371,234 sums, 368,979 of which are unique, and 166 of which
are palindromic.
-}

module Problem125
  ( problem125
  , palindromicSum
  ) where

import qualified Data.Set as Set 

problem125 :: IO ()
problem125 = print $ palindromicSum (round 1e8)

palindromicSum :: Integer -> Integer
palindromicSum = Set.foldr (+) 0 . allPCSS

-- The consecutive squares sums starting at lo^2 + (lo+1)^2 and
-- ending at lo^2 + (lo+1)^2 ... (lo+i)^2 < nMax. For example,
-- consecSqSums 100 1 = [5,14,30,55,91].
consecSqSums :: Integer -> Integer -> [Integer]
consecSqSums nMax lo =
  takeWhile (< nMax) $
  scanl (\acc i -> acc + i * i) (lo * lo + (lo + 1) * (lo + 1)) [lo + 2 ..]

-- All the consecutive squares sums. For example, allCSS 40 = sort $
-- concat [[5,14,30],[13,29],[25]], where limit = 3.
-- Use a set to efficiently remove duplicates.
allCSS :: Integer -> Set.Set Integer
allCSS nMax = Set.fromList $ concatMap (consecSqSums nMax) [1..limit] where
  limit = pred . round . sqrt . fromIntegral $ nMax `div` 2

-- All the palindromic consecutive squares sums.
allPCSS :: Integer -> Set.Set Integer
allPCSS = Set.filter isPalindrome . allCSS

isPalindrome :: Show a => a -> Bool
isPalindrome n = show n == reverse (show n)
