-- Digit cancelling fractions

{-
Data.Ratio reduces fractions to lowest terms.

Function dcf tries to cancel the 10's digit of the numerator with the
1's digit of the denominator, and vice versa, then checks the dcf for
equality with the original fraction.
-}

module Problem033
  ( problem033
  , dcf
  ) where

import Data.Char (digitToInt)
import Data.Ratio

problem033 :: IO ()
problem033 = do
  putStrLn "[(numerator, denominator, digit cancelling fraction)]:"
  print ndrTriplet
  putStrLn "reduced denominator of the product of the fractions:"
  print ans

ans :: Int
ans = denominator . product . concatMap (\(_,_,r) -> r) $ ndrTriplet

-- An intermediate result to satisfy curiosity
ndrTriplet :: [(Int, Int, [Ratio Int])]
ndrTriplet = filter (\(_,_,rs) -> rs /= [])
             [ (n, d, dcf n d) | n <- [10..98], d <- [n+1..99] ]

-- Digit cancelling fraction.
-- Given numerator n and denominator d, find 0, 1, or 2 dcfs, e.g.,
--    dcf 49 98 -> [1 % 2]
--    dcf 69 96 -> []
--       69 % 96 == 23 % 32
--       9 % 6 /= 23 % 32
--       6 % 9 /= 23 % 32
-- NB No need to check if dcf < 1, because the check is against n % d,
-- which is always < 1.
dcf :: Int -> Int -> [Ratio Int]
dcf n d = filter (== r0) [r1, r2] where
  dn = digits n
  dd = digits d
  r0 = n % d
  r1 = if dn!!0 == dd!!1 && dd!!0 /= 0 then dn!!1 % dd!!0 else 0
  r2 = if dn!!1 == dd!!0 && dd!!1 /= 0 then dn!!0 % dd!!1 else 0
  
-- Convert an integer to a list of digits.
digits :: (Integral int, Show int) => int -> [Int]
digits = map digitToInt . show

