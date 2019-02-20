-- Digit fifth powers

{-
Start the mapping from 10, because the problem constrains sums over
two or more digits.

To determine nMax, the largest possible number to satisfy the sum of
digit powers constraint, consider e digits raised to the eth power for
e <= 5:
   e * 9^e =  dpSum, some large digit power sum
   4 * 9^4 =  26244, so the largest digit power sum is from 19999,
   5 * 9^5 = 295245, so the largest digit power sum is from 199999,
so the largest dpSum = 1 + e * 9^e.
-}

module Problem030
  ( problem030
  , sumDigitPowers
  ) where

problem030 :: IO ()
problem030 = print $ sumDigitPowers 5

-- The sum of all numbers that can be written as the sum of the "e"th
-- powers of their digits.
sumDigitPowers :: Integral a => a -> a
sumDigitPowers e =
  sum .
  map fst .
  filter (\(n,ds) -> n == (sum $ map (^e) ds)) .
  map (\n -> (n, digits n)) $
  [10..nMax]
  where nMax = 1 + e * 9^e

digits :: Integral a => a -> [a]
digits = reverse . digits'
  where digits' n
          | n < 10 = [n]
          | otherwise = r : digits' q
          where (q,r) = quotRem n 10
