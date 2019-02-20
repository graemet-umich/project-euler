module Problem030Spec where

import Test.Hspec
import Problem030

spec :: Spec
spec = do
  describe "sum of digit nth powers" $ do
    let n = 4
        ans = 19316
    it ("sum of digit " ++ (show n) ++ "th powers = " ++ (show ans)) $ do
      sumDigitPowers n `shouldBe` ans
