-- abc-hits

{-

-}

module Problem127
  ( problem127
  , is_abc_hit
  , abc_hits
  , sum_cs_abc_hits
  ) where

import Data.Numbers.Primes
import qualified Data.Set as Set 

problem127 :: IO ()
problem127 = print $ 42

type ABC = (Integer, Integer, Integer)

is_abc_hit :: ABC -> Bool
is_abc_hit (a,b,c) = True

-- Satisfies predicates 2 and 3.
-- length is 249001 for cMax= 10000
--           3.6G   for cMax=120000
-- Must reduce abc space.
abcs :: Integer -> [ABC]
abcs cMax = [ (a, b, a+b) | a <- [1 .. half_cMax],
                            b <- [a+1 .. cMax-1-a] ]
  where
    half_cMax = cMax `div` 2 - 1


abc_hits :: Integer -> [ABC]
abc_hits cMax = []

sum_cs_abc_hits :: Integer -> Integer
sum_cs_abc_hits cMax = 2748
