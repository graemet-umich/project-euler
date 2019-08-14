-- Non-bouncy numbers

{-

This problem could be solved with dynamic programming, but let's
consider the binomial coefficient instead.

Let's write out some small values for the number of non-bouncy
numbers, where the rows represent the number of digits in the number,
and the columns represent the last digit of the number. For example,
there is only 1 2-digit decreasing number ending in a 9 (99), but
there are 9 2-digit decreasing numbers ending in a 0 ([10, 20 .. 90]).

Decreasing numbers:
  \   last digit | 0   1   2   3   4   5   6   7   8   9
digits
  1                0   1   1   1   1   1   1   1   1   1
  2                9   9   8   7   6   5   4   3   2   1
  3               54  45  36  28  21  15  10   6   3   1
  4                      120  84  56  35  20  10   4   1
  5                              126  70  35  15   5   1

Increasing numbers:
  \   last digit | 0   1   2   3   4   5   6   7   8   9
digits
  1                0   1   1   1   1   1   1   1   1   1
  2                0   1   2   3   4   5   6   7   8   9
  3                0   1   3   6  10  15  21  28  36  45
  4                0   1   4  10  20  35  56  84 120
  5                0   1   5  15  35  70 126  

There are several observations to make:

1. No increasing number ends with 0, because it can't start with 0.

2. Columns 9-1 for decreasing numbers are identical to columns 1-9 for
   increasing numbers. Let's just calculate one table and double the
   result.

3. The rows for increasing numbers are binomial coefficients nCk,
   where n goes as the last digit (n = ld + k - 1) and k =
   digits - 1. This observation allows enumerating and summing a row for
   an intermediate row (digits) sum.

4. The decreasing number column 0 is nC9 - 1, where n = digits + 8

The number of non-bouncy numbers of length digits is twice the
intermediate row sum from (obs. 3) for increasing and decreasing
numbers plus the row entry from column 0 of the decreasing numbers
(obs. 4) minus 9 for the redundancy for "flat" numbers. A flat number
has the same digit in all of its places, for example, 444.

To find the answer, sum the row (digits) sums for rows 1 to 100.

-}

module Problem113
  ( problem113
  , answer
  ) where

import PE.Utils (nCk)

problem113 :: IO ()
problem113 = print $ answer 100

answer :: Integral i => i -> i
answer maxPow = sum $ map digitsSum [1 .. maxPow]

-- The sum of all non-bouncy numbers with pow digits.
digitsSum :: Integral i => i -> i
digitsSum pow = sum0 + 2 * sum1to9 - 9
  where sum0 = nCk (pow + 8) 9 - 1
        sum1to9 = sum $ map (\ld -> nCk (ld + k - 1) k) [1..9]
        k = pow - 1
        
