-- Disc game prize fund

{-
The greater the number of turns, the greater the prize fund must be.

The probability of winning a 4 turn game is the sum of the possible
winning drawings, i.e. 4 blue disks or 3 blue disks is

   b1 b2 b3 b4 = 1/2 1/3 1/4 1/5 = 1/120
   r1 b2 b3 b4 = 1/2 1/3 1/4 1/5 = 1/120
   b1 r2 b3 b4 = 1/2 2/3 1/4 1/5 = 2/120
   b1 b2 r3 b4 = 1/2 1/3 3/4 1/5 = 3/120
   b1 b2 b3 r4 = 1/2 1/3 1/4 4/5 = 4/120

which is 11/120, where the bi and ri are the probilities of the ith
disc drawn being blue or red.

The maximum prize fund is (floor 120/11) = £10.
-}

module Problem121
  ( problem121
  , prizeFund
  ) where

problem121 :: IO ()
problem121 = print $ prizeFund 15

prizeFund :: Integral int => int -> int
prizeFund n = floor $ denom / numer where
  numer = fromIntegral . sum . map (chooseDisc n) $ [quot n 2 + 1 .. n]
  denom = fromIntegral $ product [2 .. n + 1]

-- The number of ways to choose k blue discs in n turns.
chooseDisc :: Integral a => a -> a -> a
chooseDisc n k
  | k == n    = 1
  | k == 0    = drawRed
  | otherwise = drawBlue + drawRed
  where drawBlue =     chooseDisc (n - 1) (k - 1) 
        drawRed  = n * chooseDisc (n - 1) k
  
