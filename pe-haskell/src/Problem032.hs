-- Pandigital products

{-
There are two pandigital product cases for m1 * m2 = p:
   d * dddd = dddd
   dd * ddd = dddd,
where
   2 <= m1 <= 99, rem m1 10 /= 0, rem m1 11 /= 0,
because 0's and repeated digits are not allowed.

Let the pandigits be 1, 2, ..., 9. Given m1, find all possible m2s:
   let pds the panDigits except for the digits in m1, e.g.,
      pds = [3..9] if m1 = 12
   candidate m2s are the 3 or 4 digit permutations of pds (function nPkEnum).

Check each m1 m2 pair if they satisfy the pandigital product property,
then sum the valid unique products.
-}

module Problem032
  ( problem032
  , pandProd
  ) where

import Data.Char (digitToInt)
import Data.List ((\\), sort)
import qualified Data.Set as Set
import PE.Utils (digits, digitsToInt, nPkEnum)

panDigits = [1..9]

problem032 :: IO ()
problem032 = print sumPandProds

sumPandProds :: Int
sumPandProds = Set.foldr (+) 0 . Set.fromList $
               [ pandProd m1 m2 | m1 <- [2..99], m2 <- m2s m1,
                                  rem m1 10 /= 0,
                                  rem m1 11 /= 0 ]
  where
    -- given m1, return all possible m2s
    m2s m1 = map digitsToInt $ nPkEnum dm2 m2Len where
      dm1 = digits m1
      dm2 = panDigits \\ dm1
      m2Len = 5 - length dm1

-- If m1 * m2 = p satisfies the pandigital property, return p else
-- return 0.
pandProd :: Int -> Int -> Int
pandProd m1 m2
  | sort (dm1 ++ dm2 ++ dp) == panDigits = p
  | otherwise = 0
  where dm1 = digits m1
        dm2 = digits m2
        p = m1 * m2
        dp  = digits p
