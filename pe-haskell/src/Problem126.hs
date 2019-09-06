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

For cuboid (a, b, c), ni is the number of cubes in layer i.

n0 = 2*(ab + ac + bc)
n1 = n1 + 4(a + b + c)
n2 = n1 + 8(a + b + c) + 8
...
ni = n1 + 4i(a + b + c) + 8(i - 1)i/2

For cuboid (3,2,1), these layers contain 22, 46, 78, and
4i^2 + 20i + 22 cubes.

From inspecting the first three layers of (a, b, c), the three
summands of ni can be interpreted geometrically.

Layer i=0 just covers the exterior of the cuboid; this is always the
face-centered components of subsequent layers.

The next term, 4i(a+b+c), is like 3 staircases a, b, and c cubes
wide. In the test case illustration, one staircase (width a=3)
descends down the z axis and up the y axis axes from the face-centered
(3,2) rectangle to the face-centered (3,1) rectangle. From symmetry,
there are four such (a) staircases and four (b) and four (c)
staircases, hence the coefficient 4 in this term. Each successive
layer adds another step to the staircases.

The last term is the corner case, literally. Let's say an initial
cuboid is convex. The first layer (i=0) is mostly convex, with the only
concavities formed by the intersection of two planes, which form the
staircases from the second term. It is only in the second (i=1) and
subsequent layers that concavities form from the intersection of three
planes at the 8 corners.

For a given corner, layer i=1 is missing 1 cube and layer i=2 3
cubes. Hence layer i=2 must cover the missing corner cube in layer
i=1, and layer i=3 must cover the 3 missing corner cubes in layer
i=2. This corner progression is the triangular number sequence, and
the coefficient 8 comes from the 8-fold corner symmetry. (All layers
for all cuboids can be fit to quadrics of form 4i^2 + ..., and this
quadratic term comes from these corner cases: 8 * i(i+1)/2).

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
    nMin | nCuboids < 10 = 6 | otherwise = (3 * nMax) ` div` 4
    h = hist (nMin, nMax)
    ih = map (\(x,y) -> (y,x)) . IntMap.toList $ h
    n
      | Just n' <- lookup nCuboids ih = n'
      | Nothing <- lookup nCuboids ih = 0

-- The number of distinct cuboids with n cubes in one of their layers.
_C :: Int -> Int
_C n = hist (n, n) IntMap.! n

-- Bin the counts where key = nCubes in layer and val = number of
-- distinct layers.
hist :: (Int, Int) -> IntMap.IntMap Int
hist = layerCounts .
       flip zip [1,1..] .
       enumTruncLayers

-- Directly calculate Cuboid limits to not exceed a cuboid first layer
-- having more than nMax cubes.
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

enumTruncLayers :: (Int, Int) -> [Int]
enumTruncLayers (nMin, nMax) =
  concatMap (dropWhile (< nMin) . takeWhile (<= nMax) . enumLayerSizes) $
  cuboids nMax

-- Generate the histogram in a map where IntMap.fromListWith is O(32 n).
-- TODO try using an array where Array.accum might have lower O(n).
layerCounts :: [(IntMap.Key, Int)] -> IntMap.IntMap Int
layerCounts = IntMap.fromListWith (+)

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
