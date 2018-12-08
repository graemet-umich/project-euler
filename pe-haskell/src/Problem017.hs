-- Number letter counts

{-

To solve this problem, one need not ever write out any number as words
... just use an association list (or map) of number n to the number of
letters in n written out as words.

Also, this problem can be solved by hand:
* In the 1's place, numbers 1-9 (36 letters) occur 9 times in 100 (324
  letters) and 90 times in 1000 (3240 letters).
* Numbers 10-19 (70 letters) occur 10 times in 1000 (700 letters).
* The words "twenty", "thirty", ..., "ninety" (46 letters) occur 10
  times in 100 (460 letters) and 100 times in 1000 (4600 letters).
* The word "hundred" (7 letters) occurs 900 times in 1000 (6300
  letters).
* The word "and" (3 letters) occurs 891 times in 1000 (2673 letters).
* The numbers 1-9 prefix "hundred" 100 times (3600 letters).
* The number 1000 has 11 letters.

-}

module Problem017
    ( problem017
    , numLetters
    , totLetters
    ) where

problem017 :: IO ()
problem017 = print $ totLetters 1000

-- The number of letters written out as words for number n.
numLetters :: Int -> Int
numLetters n
  | n == 1000 = 11                 -- "one thousand"
  | n >=  100 && r2 == 0 = c2 + 7  -- "* hundred"
  | n >=  100 = c2 + 10 + tens r2  -- "* hundred and"
  | otherwise = tens r2
  where (q2, r2) = quotRem n 100
        (Just c2) = lookup q2 letterCount

        tens n'
          | n' >= 20 = c1 + c0
          | otherwise = c
          where (q1, q0) = quotRem n' 10
                (Just c1) = lookup (10 * q1) letterCount
                (Just c0) = lookup q0 letterCount
                (Just c ) = lookup n' letterCount

-- The total number of letters written out as words for numbers 1
-- through n inclusive.
totLetters :: Int -> Int
totLetters n = sum . map numLetters $ [1 .. n]

-- An association list of number n to the number of letters in n when
-- written out as words. The number 0 is never written out as "zero."
letterCount = [( 0, 0),
               ( 1, 3), ( 2, 3), ( 3, 5),
               ( 4, 4), ( 5, 4), ( 6, 3),
               ( 7, 5), ( 8, 5), ( 9, 4), 
               (11, 6), (12, 6), (13, 8),
               (14, 8), (15, 7), (16, 7),
               (17, 9), (18, 8), (19, 8),
               (10, 3), (20, 6), (30, 6),
               (40, 5), (50, 5), (60, 5),
               (70, 7), (80, 6), (90, 6)]
