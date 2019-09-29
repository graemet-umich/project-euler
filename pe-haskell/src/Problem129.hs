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

problem129 :: IO()
problem129 = print $ iAexceeds 1000000
  
iAexceeds :: Integer -> Integer
iAexceeds kMin = fst . head
                 . dropWhile (\(_, k) -> k <= kMin)
                 . map (\n -> (n, _A n))
                 $ nsOver kMin

_A :: Integer -> Integer
_A n = go 1 1 where
  go k r
    | rmn == 0 = k
    | otherwise = go (k + 1) (10 * rmn + 1)
    where rmn = r `mod` n

-- n >= nMin >= 0 | gcd(10,n) = 1
nsOver :: Integer -> [Integer]
nsOver nMin = dropWhile (<= nMin) .
              concatMap (\i -> map (+ i) [1,3,7,9]) $
              [n10, n10 + 10 ..]
  where n10 = 10 * (nMin `div` 10) 
