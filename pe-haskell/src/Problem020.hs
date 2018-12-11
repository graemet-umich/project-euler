-- Factorial digit sum

module Problem020
    ( problem020
    , factDigitSum
    ) where

problem020 :: IO ()
problem020 = print $ factDigitSum 100

factDigitSum :: Integral a => a -> a
factDigitSum = digitSum . factorial

digitSum :: Integral a => a -> a
digitSum 0 = 0
digitSum n = r + digitSum q where (q, r) = quotRem n 10

factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)
