module Problem016Spec where

import Test.Hspec
import Problem016

spec :: Spec
spec = do
  describe "the power digit sum of n (pds n) is" $ do
    it "pds 0 == 1" $ do
      pds 0 `shouldBe` 1
    it "pds 1 == 2" $ do
      pds 1 `shouldBe` 2
    it "pds 15 == 26" $ do
      pds 15 `shouldBe` 26
