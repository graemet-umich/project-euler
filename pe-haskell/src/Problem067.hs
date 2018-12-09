-- Maximum path sum II
-- See problem 18: Maximum path sum I
-- There is no need to test this problem.

module Problem067
    ( problem067
    , maxPathSum
    ) where

import Problem018 (maxPathSum, parse)

problem067 :: IO ()
problem067 = do
  contents <- readFile "../data/p067_triangle.txt"
  let tri = parse contents
  print $ maxPathSum tri

split :: Char -> String -> [String]
split c s = split' s []
  where split' s acc
          | s  == "" = acc
          | s  == [c] = acc ++ [""] ++ [""]  -- from more than 1 trailing c
          | s2 == "" = acc ++ [s1]  -- end
          | otherwise = split' (tail s2) (acc ++ [s1])
          where s1 = takeWhile (/= c) s
                s2 = dropWhile (/= c) s
