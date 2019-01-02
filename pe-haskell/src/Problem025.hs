-- 1000-digit Fibonacci number

{-
The ratio of adjacent Fibonacci terms F(i) and F(i+1) converge to
     phi = F(i+1) / F(i),
where
     phi = (1 + sqrt(5)) / 2
is the Golden ratio.

Consider the equalities/approximations
     F(2) = F(1)
     F(3) ~ phi F(2)
     F(4) ~ phi^2 F(2)
     F(i) ~ phi^(i-2) F(2)
where
     F(2) = F(1) = 1.

This problem asks for the i where F(i-1) < 10^999 and F(i) >= 10^999, so
     F(i) >= 10^999
     phi^(i-2) >= 10^999
     i >= 999 / log(phi) + 2
     i = floor(999 / log(phi) + 2)
     i = 4782

The code below implements a testable function ift which returns the
index i of the first term F(i) with n digits. 
-}

module Problem025
    ( problem025
    , ift
    ) where

problem025 :: IO ()
problem025 = print $ ift 1000

-- index i of the first term F(i) with n digits 
ift :: Int -> Int
ift n =
  fst .
  head .
  dropWhile (\(_, fib) -> quot fib (10^(n - 1)) == 0) .
  zip [1..] $ fibs

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
