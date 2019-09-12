module Problem128Spec where

import Test.Hspec
import Problem128

spec :: Spec
spec = do
  describe "prime differences" $ do
    let n = 8
        pd = 3
    it ("PD(" ++ (show n) ++
        ") = " ++ (show pd) ++ ")") $ do
      _PD n `shouldBe` pd
    let n = 17
        pd = 2
    it ("PD(" ++ (show n) ++
        ") = " ++ (show pd) ++ ")") $ do
      _PD n `shouldBe` pd

  describe "The nth PD(n) = 3 tile" $ do
    let n = 10
        tile = 271
    it ("The " ++ (show n) ++
        "th tile = " ++ (show tile)) $ do
      nthTile n `shouldBe` tile

