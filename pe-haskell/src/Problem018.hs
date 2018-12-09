-- Maximum path sum I

{-

Consider a triangle with n rows, the ith row containing i numbers.

Start at the bottom row (n numbers). For each adjacent pair of numbers,
find the maximum, which results in n-1 numbers. Add the result to row
n-1. Repeat. For example:

     3        3         3         3      3      3   23
    7 4      7 4      7  4      7  4   20 19   20   
   2 4 6    2 4 6   10 13 15   13 15
  8 5 9 3   8 9 9  

-}

module Problem018
    ( problem018
    , maxPathSum
    , parse
    ) where

problem018 :: IO ()
problem018 = do
  contents <- readFile "../data/p018_triangle.txt"
  let tri = parse contents
  print $ maxPathSum tri

type Triangle = [[Int]]

parse :: String -> Triangle
parse = stringssToTriangle . map (split ' ') . lines

stringssToTriangle :: [[String]] -> Triangle
stringssToTriangle = map (\rowStr -> map (\nStr -> read nStr ::Int) rowStr)

split :: Char -> String -> [String]
split c s = split' s []
  where split' s acc
          | s  == "" = acc
          | s  == [c] = acc ++ [""] ++ [""]  -- from more than 1 trailing c
          | s2 == "" = acc ++ [s1]  -- end
          | otherwise = split' (tail s2) (acc ++ [s1])
          where s1 = takeWhile (/= c) s
                s2 = dropWhile (/= c) s

maxPathSum :: Triangle -> Int
maxPathSum = head . foldr1 addRows

addRows :: [Int] -> [Int] -> [Int]
addRows row sumRows = zipWith (+) row (maxAdjacent sumRows)

maxAdjacent :: [Int] -> [Int]
maxAdjacent ns = zipWith max ns (tail ns)
