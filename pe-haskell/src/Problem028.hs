-- Number spiral diagonals

{-
Consider the table
     i   j  d1  d2  dd
     1   0   1
     3   1   9   3   6
     5   2  25  13  12
     7   3  49  31  18
where i is the side length of the square, j is an adjusted index
     j = (i - 1)/2,
d1 is the upper right diagonal, d2 is the lower right diagonal, and
     dd = d1 - d2.
Equations for d1 and d2 are
     d1(i) = i^2
     d2(i,j) = d1(i) - 6j

Because of symmetry, the sum of the corners of the square with side
length i is
     sumC(i) = 2 (d1(i) + d2(i))
             = 2 [d1(i) + d1(i) - 6j]
             = 2 (2 i^2 - 6j)
     sumC(j) = 4 (2j + 1)^2 - 12j
             = 4 (4 j^2 + j + 1),
where j > 0.

Let's find the analytic solution.

The sum of the diagonals for square of side n where n' = (n - 1)/2 is
     sD(n) = 1 + 4 * sum[j=1 to n'] (4 j^2 + j + 1)
           = 1 + 4 * [ 4 (n' (n' + 1) (2n' + 1)) / 6 + (n' (n' + 1)) / 2 + n' ]
-}

module Problem028
  ( problem028
  , sD
  ) where

problem028 :: IO ()
problem028 = print $ sD 1001

sD n = 1 + 4 * (4 * quot (n' * (n' + 1) * (2 * n' + 1)) 6 +
                quot (n' * (n' + 1)) 2 + n')
  where n' = quot (n - 1) 2
  
