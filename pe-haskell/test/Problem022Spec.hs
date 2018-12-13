module Problem022Spec where

import Test.Hspec
import Problem022

spec :: Spec
spec = do
  describe "word values" $ do
    let w = "COLIN"
    it ("the word value of " ++ (show w)) $ do
      wordVal w `shouldBe` 53
    let w = "BAZ"
    it ("the word value of " ++ (show w)) $ do
      wordVal w `shouldBe` 29

