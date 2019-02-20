-- Distinct powers

module Problem029
  ( problem029
  , nDistinctTerms
  ) where

import qualified Data.Set as Set

problem029 :: IO()
problem029 = print $ nDistinctTerms (100, 100)
 
nDistinctTerms :: Integral a => (a, a) -> Int
nDistinctTerms (aMax, bMax) =
  Set.size . Set.fromList $ [ a^b | a <- [2 .. aMax], b <- [2 .. bMax] ]

