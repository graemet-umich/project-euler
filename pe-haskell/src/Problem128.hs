-- Hexagonal tile differences

{-

The unit vectors:
                     ( 0, 1, 1)
            (-1, 0, 1)        ( 1, 1, 0)
                     ( 0, 0, 0)
            (-1,-1, 0)        ( 1, 0,-1)
                     ( 0,-1,-1)

Sequence going up [(0,0,0), (0,1,1) ..]:
  A077588: Maximum number of regions into which the plane is divided by
  n triangles:
    1, 2, 8, 20, 38, 62, ...
    a(n) = 3n^2 - 3n + 2
    a(n) = a(n-1) + 6n - 6 (a(1) = 2)

Going ul from dl hex face:
1,3,10,23,42,67 ...
  a(n) = 3n^2 - 2n + 2, a(0) = 1


-}

module Problem128
  ( problem128,
    _PD,
    nthTile
  ) where

import Data.Array
import Data.Numbers.Primes

problem128 :: IO ()
problem128 = print $ 42

_PD :: Integer -> Int
_PD n = length . filter isPrime . map (abs . (n -)) . adjacents $ n

nthTile :: Int -> Integer
nthTile nth =  fst . head .
              drop (nth - 1) .
              -- take nth .
              filter (\(_,pd) -> pd == 3) .
              map (\n -> (n, _PD n)) $ [2..]

-- Tile n grouped by ring i
rings :: [[Integer]]
rings = [1] : map ring [1..] where
  ring i = [j0 .. jn] where
    j0 = 3*i*i - 3*i + 2
    jn = j0 + 6*i - 1

-- The six tiles adjacent to tile n
adjacents :: Integer -> [Integer]
adjacents 1 = [2..7]
adjacents n = map ij2n adj_ijs where
  (i,j) = n2ij n
  rn = 6*i
  pij = if j `mod` i /= 0
        then  (i-1, ( 1 + (j * (i-1)) `div` i) `mod` rn)
        else  (i+1, (-1 + (j * (i+1)) `div` i) `mod` (6 * (i+1)))
  adj_ijs = pij :
            [ (i-1,   0 + (j * (i-1)) `div` i),
              (i  , (j-1) `mod` rn),
              (i  , (j+1) `mod` rn),
              (i+1,   0 + (j * (i+1)) `div` i),
              (i+1,   1 + (j * (i+1)) `div` i) ]

-- tile n with coordinates (i,j)
ij2n :: (Integer, Integer) -> Integer
ij2n (i,j) = ij02n i + j

ij02n 0 = 1
ij02n i = 3*i*i - 3*i + 2

-- (i,j) coordinates of tile n
n2ij :: Integer -> (Integer, Integer)
n2ij n = (i, j) where
  i = ((floor . sqrt . fromInteger $ 12*n - 15) + 3) `div` 6
  j = n - ij02n i

-- unit direction vectors
u :: Array Integer [Integer]
-- direction:           u        ul        dl       d        dr       ur
u = listArray (0,5) [[0,1,1],[-1,0,1],[-1,-1,0],[0,-1,-1],[1,0,-1],[1,1,0]]

origin = [0,0,0]

-- "vector" addition of two lists
vadd :: Integral int => [int] -> [int] -> [int]
vadd v1 v2 = map (\i -> v1!!i + v2!!i) [0,1,2]


{-

tile n and coordinates and unit hex movement to go from n -> n+1

      hex
tile  move        trip     r, t
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

|trip| = 2i

n(0,0) = 1
n(i,j) = 3i^2 - 3i + 2 + j | i > 0, 0 <= j < 3(i+1)^2 - 3(i+1) + 2

rt(n): i = floor sqrt (12n - 15) + 3 `div` 6
       j = n - (3i^2 -3i + 2)

adjacent tiles:
  Let adj n = (n_i-1, n_i, n_i+1) refer to the number of adjacent tiles
  to tile n in rings i-1, i, and i+1.

  Now i = rn `div` 6, where rn = number of tiles in ring i, 
  so every ith tile j in ring i has only 1 adjacent ring (i-1) tile;
  all other ring i tiles have two adjacent ring (i-1) tiles. 

  For example, in ring 3, for tiles n = [20,23,26,29,32,35],
  adj n = (1,2,3); for all other tiles n in ring 3, adj n = (2,2,2).

  tile (i,j) adjacent to:
    inner ring: (i-1,  0 + j * (i-1) `div` i)
                (i-1,  1 + j * (i-1) `div` i `mod` rn) | j `mod` i /= 0 
    ith ring:   (i  , j-1 `mod` rn)
                (i  , j+1 `mod` rn)
    outer ring: (i+1, -1 + j * (i+1) `div` i `mod` rn) | j `mod` i == 0
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

  8 (2, 0):    * 
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



coord transforms:
trip(i,j) = ? (a,b,c) ?
rt(x,y,z) = ? (i,j) ?



-}
