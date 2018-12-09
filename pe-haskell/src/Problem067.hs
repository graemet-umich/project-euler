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
