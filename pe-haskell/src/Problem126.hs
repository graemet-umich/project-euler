-- Cuboid layers

{-

The two primary probems to solve are efficient enumeration of the
cuboid space and efficient enumeration of cuboid layers.

Enumeration of the cuboid space
-------------------------------
Implemented in function cuboids.

Cuboid (a,b,c) has first layer size 2(ab + ac + bc).

Find the maximum cuboid dimensions such that the first layer has no
more than nMax cubes, i.e. nMax >= 2(ab + ac + bc).

For the larger test cases, nMax = 78 (cMax = 3), nMax = 118
(cMax = 4), and nMax = 154 (cMax = 5).

cMax is found using a cube cuboid:
6cc <= nMax
cMax <= sqrt(nMax/6)

bMax:
2(bb + bc + bc) <= nMax
2bb + 4bc <= nMax
bMax <= (-4c + sqrt(16cc + 8nMax)) / 4

aMax:
2(a(b + c) + bc) <= nMax
a(b + c) <= nMax/2 - bc
aMax <= (nMax / 2 - bc) / (b + c)

Looking ahead to a solution by extrapolating linear fit lm(formula =
C(n) ~ n) over range 22 < n < 2000, fit slope=0.040838 (1/25), so n ~
1000/slope = 25000. Use this n as nMax, and if a solution for
C(n)=1000 is not found, bump nMax higher. However ...

... better testing with the ratios of (iC(nCuboids) / nCuboids), which
is linear, suggests a factor of <20 suffices.

Enumeration of cuboid layers
----------------------------
Implemented in function enumLayerSizes.



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
problem126 = print $ iC 1000

-- A cubiod (a, b, c) always satisfies a >= b >= c.
type Cuboid = (Int, Int, Int)  -- dimensions of a cuboid 
type Cube = Cuboid             -- coordinates of a cube

-- Find the smallest layer size n (number of cubes in layer) that
-- covers nCuboids distinct cuboids.
iC :: Int -> Int
iC nCuboids = n
  where
    n_to_nCuboid_ratio = 20
    nMax = n_to_nCuboid_ratio * nCuboids
    h = hist nMax
    ih = map (\(x,y) -> (y,x)) . IntMap.toList $ h
    n
      | Just n' <- lookup nCuboids ih = n'
      | Nothing <- lookup nCuboids ih = 0

-- The number of distinct cuboids with n cubes in one of their layers.
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
    
-- Given a cuboid, enumerate the number of cubes in successive cover
-- layers. This implementation is in closed form.
enumLayerSizes :: Cuboid -> [Int]
enumLayerSizes cuboid@(a,b,c) = map (+ l1) $ 0 : map fl [1..]
  where
    l1 = 2 * (a*b + a*c + b*c)
    abc = a + b + c
    fl n = 4*n*(abc + n - 1)

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
