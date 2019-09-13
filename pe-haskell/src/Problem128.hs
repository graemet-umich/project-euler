-- Hexagonal tile differences

{-

To find a better algorithm than testing predicate PD(n) = 3 for every
n > 0, I noticed (while passing the test that the 10th predicate tile
n = 271) that every n that satisfies the predicate could either be an
element in OEIS sequence A077588 or be 1 less than an element in that
sequence: [1,2,8,19,20,37,61,128,217,271]. This reduced the n search
space from 14.5e9 to 1.4e5.

Sequence going up:
  A077588: Maximum number of regions into which the plane is divided by
  n triangles:
    1, 2, 8, 20, 38, 62, 92, 128, 170, 218, 272, 332 ...
    a(n) = 3n^2 - 3n + 2, a(0) = 1

See end comments for using an (i,j) coordinate system to find the 6
tiles adjacent to tile n, where tile n is the jth tile in the ith
ring.

-}

module Problem128
  ( problem128,
    _PD,
    nthTile
  ) where

import Data.Array
import Data.Numbers.Primes
import qualified Data.Set as Set

problem128 :: IO ()
problem128 = print $ nthTile 2000

_PD :: Integer -> Int
_PD n = length . filter isPrime' . map (abs . (n -)) . adjacents $ n

-- Use set primes' to more quickly determine primes: use isPrime'
-- lookup instead of isPrime calculation.
-- 834761 is the largest prime difference for the 2000th tile.
primes' = Set.fromList . takeWhile (< 834762) $ primes
isPrime' n = n `Set.member` primes'

-- Return tile n, the nth tile with PD(n) == 3. 
nthTile :: Int -> Integer
nthTile nth = fst . head .
              drop (nth - 1) .
              filter (\(_,pd) -> pd == 3) .
              map (\n -> (n, _PD n)) $
              interleave a077588' a077588

-- The initial correct but slow algorithm that checks every n > 0.
nthTile' nth = fst . head .
              drop (nth - 1) .
              filter (\(_,pd) -> pd == 3) .
              map (\n -> (n, _PD n)) $
              [1..]

-- Interleave two lists.
interleave :: [a] -> [a] -> [a]
interleave xs = concatMap (\(x,y) -> [x,y]) . zip xs

-- OEIS A077588
a077588  = map ij02n [1..]
a077588' = map pred a077588

-- The six tiles adjacent to tile n. See below comments for adjacency
-- examples that aided formula determinations.
adjacents :: Integer -> [Integer]
adjacents 1 = [2..7]
adjacents n = map ij2n adj_ijs where
  (i,j) = n2ij n
  -- rn = 6 * i  -- omitted for reading symmetry
  pij = if j `mod` i /= 0
        then  (i-1, ( 1 + (j * (i-1)) `div` i) `mod` (6 * (i-1)))
        else  (i+1, (-1 + (j * (i+1)) `div` i) `mod` (6 * (i+1)))
  adj_ijs = pij :
            [ (i-1,   0 + (j * (i-1)) `div` i),
              (i  ,       (j-1)                `mod` (6 * i)),
              (i  ,       (j+1)                `mod` (6 * i)),
              (i+1,   0 + (j * (i+1)) `div` i),
              (i+1,   1 + (j * (i+1)) `div` i) ]

-- Tile n with coordinates (i,j)
ij2n :: (Integer, Integer) -> Integer
ij2n (i,j) = ij02n i + j

-- The tiles going up (j = 0): 1,2,8,20,38,62...
ij02n 0 = 1
ij02n i = 3*i*i - 3*i + 2

-- The (i,j) coordinates of tile n
n2ij :: Integer -> (Integer, Integer)
n2ij n = (i, j) where
  i = ((floor . sqrt . fromInteger $ 12*n - 15) + 3) `div` 6
  j = n - ij02n i


{-

Notes taken during exploration.

I abandoned the triplet coordinates after I solved for adjacent tiles
using the (i,j) coordinates.

Tile n and coordinates and unit hex movement to go from n -> n+1:

      hex
tile  move        trip     i, j
  1   u       ( 0, 0, 0)  (0, 0)

  2   dl      ( 0, 1, 1)  (1, 0)    0,2
  3   d       (-1, 0, 1)  (1, 1)    1,2
  4   dr      (-1,-1, 0)  (1, 2)   -1,2
  5   ur      ( 0,-1,-1)  (1, 3)
  6   u       ( 1, 0,-1)  (1, 4)
  7   ul u    ( 1, 1, 0)  (1, 5)
     
  8   dl      ( 0, 2, 2)  (2, 0)    0,2
  9   dl      (-1, 1, 2)  (2, 1)    1,2
 10   d       (-2, 0, 2)  (2, 2)    2,3
 11   d       (-2,-1, 1)  (2, 3)   -1,2
 12   dr      (-2,-2, 0)  (2, 4)   -2,3
 13   dr      (-1,-2,-1)  (2, 5)
 14   ur      ( 0,-2,-2)  (2, 6)
 15   ur      ( 1,-1,-2)  (2, 7)
 16   u       ( 2, 0,-2)  (2, 8)
 17   u       ( 2, 1,-1)  (2, 9)
 18   ul      ( 2, 2, 0)  (2,10)
 19   ul u    ( 1, 2, 1)  (2,11)

 20   dl      ( 0, 3, 3)  (3, 0)

trip: sum( |x|, |y|, |z| ) = 2i

n(0,0) = 1
n(i,j) = 3i^2 - 3i + 2 + j | i > 0, 0 <= j < 3(i+1)^2 - 3(i+1) + 2

ij(n): i = floor sqrt (12n - 15) + 3 `div` 6
       j = n - (3i^2 -3i + 2)

adjacent tiles:
  Let adj n = (n_i-1, n_i, n_i+1) refer to the number of adjacent tiles
  to tile n in rings i-1, i, and i+1.

  For example, in ring 3, for tiles n = [20,23,26,29,32,35],
  adj n = (1,2,3); for all other tiles n in ring 3, adj n = (2,2,2).

  Now i = rn `div` 6, where rn = number of tiles in ring i, so every
  ith tile j in ring i has only 1 adjacent ring (i-1) tile; all other
  ring i tiles have two adjacent ring (i-1) tiles. As added notation,
  let rn- = 6(i-1) and rn+ = 6(i+1).

  tile (i,j) adjacent to:
    inner ring: (i-1,  0 + j * (i-1) `div` i)
                (i-1,  1 + j * (i-1) `div` i `mod` rn-) | j `mod` i /= 0 
    ith ring:   (i  , j-1 `mod` rn)
                (i  , j+1 `mod` rn)
    outer ring: (i+1, -1 + j * (i+1) `div` i `mod` rn+) | j `mod` i == 0
                (i+1,  0 + j * (i+1) `div` i)
                (i+1,  1 + j * (i+1) `div` i)

  adjacency examples:
  2 (1, 0):
     1 (0, 0)
     3 (1, 1)
     7 (1, 5)
    19 (2,11)
     8 (2, 0)
     9 (2, 1)
  3 (1, 1):
     1 (0, 0)
     2 (1, 1)
     4 (1, 2)
     9 (2, 1)
    10 (2, 2)
    11 (2, 3)

  8 (2, 0):    
     2 (1, 0)
     9 (2, 1)
    19 (2,11)
    37 (3,17)
    20 (3, 0)
    21 (3, 1)
 17 (2, 9):
     6 (1, 4)
     7 (1, 5)
    16 (2, 8)
    18 (2,10)
    33 (3,13)
    34 (3,14)
 18 (2,10):
     7 (1, 5)
    17 (2, 9)
    19 (2,11)
    34 (3,14)
    35 (3,15)
    36 (3,16)
 19 (2,11):
     7 (1, 5)
     2 (1, 0)
    18 (2,10)
     8 (2, 0)
    36 (3,16)
    37 (3,17)

 20 (3, 0):    
     8 (2, 0)   0     j * (i-1) `div` i
    37 (3,17)
    21 (3, 1)   
    61 (4,23)  -1
    38 (4, 0)   0     j * (i+1) `div` i
    39 (4, 1)   1
 21 (3, 1):
     8 (2, 0)   0     j * (i-1) `div` i
     9 (2, 1)   1
    20 (3, 0)
    22 (3, 2)
    39 (4, 1)   0     j * (i+1) `div` i
    40 (4, 2)   1
 37 (3,17):
    19 (2,11)   0     j * (i-1) `div` i
     8 (2, 0)   1
    36 (3,16)
    20 (3, 0)
    60 (4,22)   0     j * (i+1) `div` i
    61 (4,23)   1

-}
