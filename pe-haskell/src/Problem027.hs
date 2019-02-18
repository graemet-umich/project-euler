-- Quadratic primes

{-
The equation is of the form
     p = n^2 + an + b,
and the problem asks to find consecutive primes starting from n = 0.

When n = 0, p = b, so b is constrained to be prime. There are only 168
primes less than 1000.

If b is 2, then p is even for even n. Therefore b can not be 2. 

When n = 1,
     p = 1 + a + b,
so a is constained as
     a >= 2 - b.

If a is even, then the parity of p alternates with the parity of n,
     even or odd = 0 + 0a + b(2 or odd)
     odd or even = 1 + 1a + b(2 or odd) ...
so a must be odd.

The constraints on a and b limit the equation to be evaluated 121,479
times instead of the naive 3,999,999 times.

The program runs about 5x faster by isPrime hitting a cached set of
primes < 100,000, chosen by n = 90, a = 999, and b = 997.  
-}

module Problem027
    ( problem027
    , ncpAndAB
    ) where

import Data.List (sort)
import Data.Numbers.Primes (primes)
import qualified Data.Set as Set

problem027 :: IO ()
problem027 = do
  putStrLn "(longest consecutive prime run length, a * b)"
  print ans 

-- Returns the pair (longest consecutive prime run length, a * b).
ans :: (Int, Int)
ans = maximum $
      map ncpAndAB [ (a, b) | b <- tail $ takeWhile (< 1000) primes,
                              a <- [2-b, 4-b .. 999] ]

ncpAndAB :: (Int, Int) -> (Int, Int)
ncpAndAB (a, b) = (ncp, a * b) where
  ncp = length .
        takeWhile isPrime .
        map eqn $
        [0..]
  eqn n = n * n + a * n + b
  isPrime n = Set.member n primeSet

primeSet :: Set.Set Int
primeSet = Set.fromList . tail . takeWhile (< 100000) $ primes
