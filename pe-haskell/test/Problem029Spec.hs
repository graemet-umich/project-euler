module Problem029Spec where

import Test.Hspec
import Problem029

spec :: Spec
spec = do
  describe "number of distinct terms for (aMax, bMax)" $ do
    let aMax = 5
        bMax = 5
        ans = 15
    it ("(aMax, bMax) = " ++ (show (aMax, bMax))) $ do
      nDistinctTerms (aMax, bMax) `shouldBe` ans
