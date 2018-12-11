module Problem020Spec where

import Test.Hspec
import Problem020

spec :: Spec
spec = do
  describe "the factorial digit sum" $ do
    let n = 10
    it ("of " ++ (show n)) $ do
      factDigitSum n `shouldBe` 27

