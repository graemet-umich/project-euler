module Problem130Spec where

import Test.Hspec
import Problem130

spec :: Spec
spec = do
  describe "composites" $ do
    let n = 5
        first5Comps = [91, 259, 451, 481, 703]
    it ("The first " ++ (show n) ++ " composites = " ++ (show first5Comps)) $ do
      composites n `shouldBe` first5Comps

