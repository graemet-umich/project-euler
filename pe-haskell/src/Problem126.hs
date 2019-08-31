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

{-
Keep this for now to work on a closed form solution.

Given the below examples:
1. curve fit degree 2 polynomial to first 3 points
2. find closed form solution as function of cuboid


-------
(a,1,1)
a(n) = 4*n^2 + 4*(a-1)*n + 2

(1,1,1)
Coordination sequence for cubic lattice
Recurrence: n*a(n) = (n-2)*a(n-2) + 6*a(n-1), a(0)=1, a(1)=6
Closed:     a(n)   = 4*n^2 + 2
[6,18,38,66,102,146,198,258,326,402,486,578,678,786,902]

(2,1,1)
Centered square numbers
a(n) = 8*n + a(n-1) for n > 0, a(0)=2
a(n) = 4*n^2 + 4*n + 2
     = (2*n + 1)^2 + 1
[10,26,50,82,122,170,226,290,362,442,530,626,730,842,962]

(3,1,1)
Number of right triangles of a given area required to form successively larger squares.
a(n) = 8*n + a(n-1) + 4 with n>0, a(0)=2
a(n) = 4*n^2 + 8*n + 2
[14,34,62,98,142,194,254,322,398,482,574,674,782,898]

(4,1,1)
a(n) = 4*n^2 + 12*n + 2
[18,42,74,114,162,218,282,354,434,522,618,722,834,954]


-------
(a,2,1)
a(n) = 4*n^2 + 4*a*n + a*b

(2,2,1)
Even squares
a(n) = a(n-1) + 8*n - 4, a(0)=0
a(n) = 4*n^2 + 8*n + 4
[16,36,64,100,144,196,256,324,400,484,576,676,784,900]

(3,2,1)
a(n) = 4*n^2 + 12*n + 6
[22,46,78,118,166,222,286,358,438,526,622,726,838,958]

(4,2,1)
a(n) = 4*n^2 + 16*n + 8
[28,56,92,136,188,248,316,392,476,568,668,776,892]

(5,2,1)
a(n) = 4*n^2 + 20*n + 10
[34,66,106,154,210,274,346,426,514,610,714,826,946]


-------
(a,3,1)
a(n) = 4*n^2 + 4*(a+1)*n + a*b + (a-2)

(3,3,1)
a(n) = 4*n^2 + 16*n + 10
[30,58,94,138,190,250,318,394,478,570,670,778,894]

(4,3,1)
a(n) = 4*n^2 + 20*n + 14
[38,70,110,158,214,278,350,430,518,614,718,830,950]

(5,3,1)
a(n) = 4*n^2 + 24*n + 18
[46,82,126,178,238,306,382,466,558,658,766,882]




(2,2,2)
8 times triangular numbers
a(n) = a(n-1) + 8*n, a(0)=0
a(n) = 4*n*(n+1)
     = (2*n+1)^2 - 1
[24,48,80,120,168,224,288,360,440,528,624,728,840,960]

(3,2,2)
a(n) = a(n-1) + 8*n - 4, a(1)=0
a(n) = (2*n)^2 - 4
[32,60,96,140,192,252,320,396,480,572,672,780,896]




(3,3,2)
a(n) = 4*n^2 + 20*n + 18
[42,74,114,162,218,282,354,434,522,618,722,834,954]

(3,3,3)
a(n) = 4*n^2 + 24*n + 26
[54,90,134,186,246,314,390,474,566,666,774,890]

-}

-- Given a cuboid, enumerate the number of cubes in successive cover
-- layers.
-- Start mapping at n=1 instead of n=4, which effectively tests
-- function fitQuad via the test of enumLayerSizes. (There is one test
-- for the 1st four layers of a cuboid, and there are four tests for
-- the 1st layer of a cuboid.)
enumLayerSizes :: Cuboid -> [Int]
enumLayerSizes cuboid = map (\n -> a*n*n + b*n +c) [1..] where
  first3 = take 3 . map Set.size . iterate nextLayer . firstLayer $ cuboid
  (a,b,c) = fitQuad first3
  -- let's not do:
  -- first3 ++ map (\n -> a*n*n + b*n + c) [4..]

-- Find the first 3 terms with take 3 (enumLayerSizes cuboid).
-- Fit the 1st 3 terms to a quadratic sequence a(n) = a*n^2 + b*n + c.
--   a(1) =  a +  b + c
--   a(2) = 4a + 2b + c
--   a(3) = 9a + 3b + c
-- Use (a,b,c) coefficients to quickly enumerate subsequent layer
-- sizes.
fitQuad :: [Int] -> (Int, Int, Int)
fitQuad (a1:a2:a3:[]) = (a, b, c) where
  a = (a3 - 2 * a2 + a1) `div` 2
  b = a2 - a1 - 3*a
  c = a1 - a - b

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
firstLayer cuboid = layer Set.\\ cubes
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
  
-- Return the surface cubes of the initial cuboid measuring a x b x c;
-- for cuboids with c < 3, this is the entire cuboid.
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
