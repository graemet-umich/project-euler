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
problem126 = print $ 42

type Cuboid = (Int, Int, Int)

enumLayers :: Cuboid -> [Int]
enumLayers cuboid = [22]

_C :: Int -> Int
_C n = 2

iC :: Int -> Int
iC nCuboids = 42    
