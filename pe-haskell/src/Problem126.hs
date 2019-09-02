-- Cuboid layers

{-

Cuboid (a,b,c) has first layer size 2(ab + ac + bc)

Find the maximum cuboid dimensions no greater than nMax cubes.
cMax:
6cc = nMax
cMax = sqrt(nMax/6)

bMax:
2(bb + bc + bc) = nMax
2bb + 4bc = nMax
bMax = (-4c + sqrt(16cc + 8nMax)) / 4

aMax:
a(b + c) = n/2 - bc
aMax = (nMax / 2 - bc) / (b + c)

For the test cases, nMax = 54 -> cMax = 5. 

Looking ahead to a solution by extrapolating linear fit lm(formula =
C(n) ~ n) over range 22 < n < 2000, fit slope=0.040838, so n ~
1000/slope = 24487. Use this n as nMax, and if a solution for
C(n)=1000 is not found, bump nMax higher.

-}

module Problem126
  ( problem126
  , _C
  , iC
  , enumLayerSizes
  ) where

import qualified Data.IntMap as IntMap
import qualified Data.Set as Set

problem126 :: IO ()
problem126 = print $ _C 500  -- iC 1000

-- A cubiod (a, b, c) always satisfies a >= b >= c.
type Cuboid = (Int, Int, Int)  -- dimensions of a cuboid 
type Cube = Cuboid             -- coordinates of a cube
type Coefs = Cuboid            -- coefficients of fitted quadratic

-- λ> lookup 1 (map (\(x,y) -> (y,x)) . IntMap.toList $ hist 154)
-- Just 6
iC :: Int -> Int
iC nCuboids = 2

{-
This works but:
Test case C(154)=10 -> 0.4".
C(1000) = 54 -> 39".
C(2000) = 84 -> 3'30"
Need C(n) = 1000
-}
_C :: Int -> Int
_C n = hist n IntMap.! n

-- Bin the counts where key = nCubes in layer and val = number of
-- distinct layers.
hist :: Int -> IntMap.IntMap Int
hist = layerCounts .
       flip zip [1,1..] .
       concatMap snd .
       concat .
       concat .
       cs

-- Directly calculate Cuboid limits to not exceed a cuboid first layer
-- having more than nMax cubes.
-- To partially replace as,bs,cs functions.
cuboids :: Int -> [Cuboid]
cuboids nMax = [ (a,b,c) |
                c <- [1..cMax],
                let bMax = (-4*c + rsqrti (16*c*c + 8*nMax)) `div` 4,
                b <- [c .. bMax],
                let aMax = ((nMax `div` 2) - b*c) `div` (b + c),
                a <- [b .. aMax] ]
  where
    cMax = round . sqrt $ (fromIntegral nMax) / 6
    sqrti = sqrt . fromIntegral
    rsqrti = round . sqrti

-- Loop through all (a,b,c) cuboids and their layers with nCubes no
-- greater than n.
--as :: Int -> Int -> Int -> [(Cuboid, [Int])]
as n c b =
  takeWhile (\(cuboid, layers) -> layers /= []) .
  map (\cuboid -> (cuboid, takeWhile (<= n) $ enumLayerSizes cuboid)) .
  map (\a -> (a,b,c)) $
  [b..]
bs :: Int -> Int -> [[(Cuboid, [Int])]]
bs n c = takeWhile (/= []) $ map (as n c) [c..]
cs :: Int -> [[[(Cuboid, [Int])]]]
cs n = takeWhile (/= []) $ map (bs n) [1..]

-- Generate the histogram in a map where IntMap.fromListWith is O(32 n).
-- TODO try using an array where Array.accum might have lower O(n).
layerCounts :: [(IntMap.Key, Int)] -> IntMap.IntMap Int
layerCounts = IntMap.fromListWith (+)

addLayerCounts :: IntMap.IntMap Int -> [(IntMap.Key, Int)] -> IntMap.IntMap Int
addLayerCounts m = IntMap.unionWith (+) m . IntMap.fromListWith (+) 
    

{-
TODO low priority but interesting:
Keep this for now to work on a closed form solution.
SOME of these closed forms are wrong.


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
a(n) = 4*n^2 + 12*n + 8
[24,48,80,120,168,224,288,360,440,528,624,728,840,960]

(3,2,2)
a(n) = a(n-1) + 8*n - 4, a(1)=0
a(n) = 4*n^2 + 16*n + 12
[32,60,96,140,192,252,320,396,480,572,672,780,896]


(3,3,2)
a(n) = 4*n^2 + 20*n + 18
[42,74,114,162,218,282,354,434,522,618,722,834,954]

(3,3,3)
a(n) = 4*n^2 + 24*n + 26
[54,90,134,186,246,314,390,474,566,666,774,890]

-}

-- TODO NEXT need to find large layers fast.
-- Don't need to enumerate, just solve for n.

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


-- TODO next 3 functions. Find better algorithm to get layer (or
-- cuboid shell) directly, without set diff'ing away the previous
-- layer or cuboid interior.

-- Project along 3 axes new cubes onto layer. 
nextLayer :: Set.Set Cube -> Set.Set Cube
nextLayer layer = Set.unions [newposxs, newnegxs,
                              newposys, newnegys,
                              newposzs, newnegzs]
                  Set.\\ layer
  where  -- TODO switch to 3-element list and pass projection axis as 0, 1, or 2 
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
-- Builds a 100^3 cuboid in 0.1 sec.
buildCuboid :: Cuboid -> Set.Set Cube
buildCuboid (a,b,c)
  | a < b = error "buildCuboid (a,b,c): a < b"
  | b < c = error "buildCuboid (a,b,c): b < c"
  | otherwise = Set.unions . map Set.fromList $ [pxs,pys,pzs,nxs,nys,nzs]
  where
    x = a `div` 2
    y = b `div` 2
    z = c `div` 2
    pxs = [ ( x,  j,  k) | j <- getRange b, k <- getRange c ] 
    pys = [ ( i,  y,  k) | i <- getRange a, k <- getRange c ] 
    pzs = [ ( i,  j,  z) | i <- getRange a, j <- getRange b ] 
    nxs = map (\(i,j,k) -> (-i,j,k)) pxs
    nys = map (\(i,j,k) -> (i,-j,k)) pys
    nzs = map (\(i,j,k) -> (i,j,-k)) pzs
    
getRange :: Int -> [Int]
getRange x
  | odd x = 0 : [1..halfx] ++ map (* (-1)) [1..halfx]
  | otherwise = [1..halfx] ++ map (* (-1)) [1..halfx]
  where
    halfx = x `div` 2
