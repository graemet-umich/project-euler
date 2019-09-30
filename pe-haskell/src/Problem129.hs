-- Repunit divisibility

{-

The ns can only end in 1, 3, 7, or 9, because gcd(1,n) = 1.

Use long division to find A(n).

n > A(n) for all n except n = 3^i. For the solution, n >= 1e6 + 1.

-}

module Problem129
  ( problem129
  , _A
  , iAexceeds
  ) where

import PE.Repunit (_A, nsOver)

problem129 :: IO()
problem129 = print $ iAexceeds 1000000
  
iAexceeds :: Integer -> Integer
iAexceeds kMin = fst . head
                 . dropWhile (\(_, k) -> k <= kMin)
                 . map (\n -> (n, _A n))
                 $ nsOver kMin
