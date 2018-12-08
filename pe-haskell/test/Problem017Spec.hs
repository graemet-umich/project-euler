module Problem017Spec where

import Test.Hspec
import Problem017

spec :: Spec
spec = do
  describe "the number of letters used" $ do
    it "1" $ do
      numLetters 1 `shouldBe` 3
    it "115" $ do
      numLetters 115 `shouldBe` 20
    it "342" $ do
      numLetters 342 `shouldBe` 23
    it "1 to 5" $ do
      totLetters 5 `shouldBe` 19
