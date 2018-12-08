-- Maximum path sum II
-- See problem 18: Maximum path sum I
-- There is no need to test this problem.

module Problem067
    ( problem067
    , maxPathSum
    ) where

problem067 :: IO ()
problem067 = do
  contents <- readFile "../data/p067_triangle.txt"
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
