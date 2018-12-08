module Problem018Spec where

import Test.Hspec
import Problem018

spec :: Spec
spec = do
  describe "the maximum path sum" $ do
    let tri4 = [[3], [7,4], [2,4,6], [8,5,9,3]]
    it "test triangle with 4 rows" $ do
      maxPathSum tri4 `shouldBe` 23

