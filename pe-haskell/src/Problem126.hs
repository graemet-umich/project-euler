-- Cuboid layers

{-

-}

module Problem126
  ( problem126
  , _C
  , iC
  , enumLayers
  ) where

import qualified Data.Set as Set 

problem126 :: IO ()
problem126 = print $ iC 1000

type Cuboid = (Int, Int, Int)  -- dimensions of a cuboid 
type Cube = Cuboid             -- coordinates of a cube 

iC :: Int -> Int
iC nCuboids = 42

_C :: Int -> Int
_C n = 2

enumLayers :: Cuboid -> [Int]
enumLayers cuboid = [22]

-- Return the cubes of the initial cuboid.
-- Note for even cuboid sides, there is no 0, which leverages symmetry. 
buildCuboid :: Cuboid -> Set.Set Cube
buildCuboid (a,b,c) = Set.fromList [ (x,y,z) | x <- xs, y <- ys, z <- zs ]
  where
    xs = getRange a
    ys = getRange b
    zs = getRange c

getRange :: Int -> [Int]
getRange x
  | odd x = 0 : [1..halfx] ++ map (* (-1)) [1..halfx]
  | otherwise = [1..halfx] ++ map (* (-1)) [1..halfx]
  where
    halfx = x `div` 2

