-- Digit factorials

{-
The largest number to test is 1,999,999, because its digit factorial is
   1 + 6 * 9! = 2,177,281,
and
   1 + 5 * 9! + 8! = 1,854,721
is not large enough.

However, the largest digit factorial, 40585, is far smaller.
-}

module Problem034
  ( problem034
  , getDigitFactorial
  ) where

import Data.Array
import PE.Utils (digits)

problem034 :: IO ()
problem034 = do
  putStrLn "digit factorials (problem statement excludes single digits):"
  print digitFactorials
  putStrLn "sum:"
  print $ sum . filter (> 9) $ digitFactorials

factorial :: Array Int Integer
factorial = listArray (0, 9) $ 1 : map (\n -> product [1..n]) [1..9]
            {- O(n) overkill for small array
            reverse . foldr (\d (f:fs) -> d*f:f:fs) [1] $ [9,8..1]
            -}

-- Empirically determined
maxN = 40585

digitFactorials :: [Integer]
digitFactorials = filter (/= 0) . map getDigitFactorial $ [0..maxN]

getDigitFactorial :: Integer -> Integer
getDigitFactorial n
  | n == (sum . map (factorial!) . digits) n = n
  | otherwise = 0
