-- Names scores

module Problem022
    ( problem022
    , wordVal
    ) where

import Data.Char (ord)
import Data.List (sort)
import PE.Utils (split)

problem022 :: IO ()
problem022 = do
  contents <- readFile("../data/p022_names.txt")

  print .
    sum .
    map (\(alphaPos, wordVal) -> alphaPos * wordVal) .
    zip [1..] .
    map wordVal .
    sort .
    split ',' .
    filter (/= '"') $
    contents

wordVal :: String -> Int
wordVal = foldr (\c val -> val + ord c - ord 'A' + 1) 0  

