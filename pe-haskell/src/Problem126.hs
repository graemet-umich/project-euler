-- Cuboid layers

{-

-}

module Problem126
  ( problem126
  , _C
  , iC
  , enumLayerSizes
  ) where

import qualified Data.Set as Set

problem126 :: IO ()
problem126 = print $ iC 1000

-- A cubiod (a, b, c) always satisfies a >= b >= c.
type Cuboid = (Int, Int, Int)  -- dimensions of a cuboid 
type Cube = Cuboid             -- coordinates of a cube 

iC :: Int -> Int
iC nCuboids = 42

_C :: Int -> Int
_C n = 2

enumLayerSizes :: Cuboid -> [Int]
enumLayerSizes = map Set.size . iterate nextLayer . firstLayer

-- Project along 3 axes new cubes onto layer. 
nextLayer :: Set.Set Cube -> Set.Set Cube
nextLayer layer = Set.unions [newposxs, newnegxs,
                              newposys, newnegys,
                              newposzs, newnegzs]
                  Set.\\ layer
  where  -- TODO switch to 3-element array and pass projection axis as 0, 1, or 2 
    nnxs =  Set.filter (\(x,_,_) -> x >= 0) layer
    newposxs = Set.map (\(x,y,z) -> (  x+1 ,y,z)) nnxs
    newnegxs = Set.map (\(x,y,z) -> (-(x+1),y,z)) nnxs

    nnys =  Set.filter (\(_,y,_) -> y >= 0) layer
    newposys = Set.map (\(x,y,z) -> (x,  y+1 ,z)) nnys
    newnegys = Set.map (\(x,y,z) -> (x,-(y+1),z)) nnys

    nnzs =  Set.filter (\(_,_,z) -> z >= 0) layer
    newposzs = Set.map (\(x,y,z) -> (x,y,  z+1 )) nnzs
    newnegzs = Set.map (\(x,y,z) -> (x,y,-(z+1))) nnzs

firstLayer :: Cuboid -> Set.Set Cube
firstLayer cuboid@(a,b,c) = layer Set.\\ cubes
  where
    cubes = buildCuboid cuboid
    layer  = nextLayer cubes

{- private
da22 :: Set.Set Cube
da22 = Set.fromList [(2,1,0), (-2,1,0), (2,-1,0), (-2,-1,0),
                     (-1,2,0),(0,2,0),(1,2,0),(-1,-2,0),(0,-2,0),(1,-2,0),
                     (-1,1,1),(0,1,1),(1,1,1),(-1,-1,1),(0,-1,1),(1,-1,1),
                     (-1,1,-1),(0,1,-1),(1,1,-1),(-1,-1,-1),(0,-1,-1),(1,-1,-1)]

da46 :: Set.Set Cube
da46 = Set.fromList [(3,1,0), (3,-1,0),(-3,1,0), (-3,-1,0),
                     (2,1,1), (2,-1,1),(2,1,-1), (2,-1,-1),(2,2,0),(2,-2,0),
                     (-2,1,1), (-2,-1,1),(-2,1,-1), (-2,-1,-1),(-2,2,0),(-2,-2,0),
                     (1,3,0), (1,-3,0),
                     (1,2,1), (1,-2,1), (1,2,-1), (1,-2,-1),
                     (1,1,2), (1,-1,2), (1,1,-2), (1,-1,-2), 
                     (-1,3,0), (-1,-3,0),
                     (-1,2,1), (-1,-2,1), (-1,2,-1), (-1,-2,-1),
                     (-1,1,2), (-1,-1,2), (-1,1,-2), (-1,-1,-2)
                    ]
-}
  
-- Return the surface cubes of the initial cuboid; for cuboids with
-- c < 3, this is the entire cuboid.
-- Note for even cuboid sides, there is no 0, which leverages symmetry.
-- [The "solid - interior" method is easy but does not scale
-- well, e.g. 100^3 cuboid takes 4+ sec to build.]
buildCuboid :: Cuboid -> Set.Set Cube
buildCuboid (a,b,c)
  | a < b = error "buildCuboid (a,b,c): a < b"
  | b < c = error "buildCuboid (a,b,c): b < c"
  | c < 3 = solid
  | otherwise = layer
  where
    xs = getRange a
    ys = getRange b
    zs = getRange c
    solid = Set.fromList [ (x,y,z) | x <- xs, y <- ys, z <- zs ]
    xs' = getRange (a - 2)
    ys' = getRange (b - 2)
    zs' = getRange (c - 2)
    interior = Set.fromList [ (x',y',z') | x' <- xs', y' <- ys', z' <- zs' ]
    layer = solid Set.\\ interior
      

getRange :: Int -> [Int]
getRange x
  | odd x = 0 : [1..halfx] ++ map (* (-1)) [1..halfx]
  | otherwise = [1..halfx] ++ map (* (-1)) [1..halfx]
  where
    halfx = x `div` 2
