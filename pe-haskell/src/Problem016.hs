-- Power digit sum

module Problem016
    ( problem016
    , pds
    ) where

import Data.Char (digitToInt)

problem016 :: IO ()
problem016 = print $ pds 1000

pds :: Int -> Int
pds = sum . map digitToInt . show . (^) 2

