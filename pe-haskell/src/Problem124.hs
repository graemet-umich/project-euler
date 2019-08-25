-- Ordered radicals

{-

-}

module Problem124
  ( problem124
  , _E
  ) where

import Data.Numbers.Primes (primes)

problem124 :: IO ()
problem124 = print $ _E 100000 10000

_E nMax k = 8

