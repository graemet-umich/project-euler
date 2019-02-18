module Problem028Spec where

import Test.Hspec
import Problem028

spec :: Spec
spec = do
  describe "diagonal sum for square of side length n" $ do
    let n = 1
        ans = 1
    it ("n = " ++ (show n)) $ do
      sD n `shouldBe` ans
    let n = 3
        ans = 25
    it ("n = " ++ (show n)) $ do
      sD n `shouldBe` ans
    let n = 5
        ans = 101
    it ("n = " ++ (show n)) $ do
      sD n `shouldBe` ans

